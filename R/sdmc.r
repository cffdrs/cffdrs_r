sdmc<-function(input,sdmc_old=NULL,batch=TRUE){
  names(input) <- tolower(names(input))
  input<-input[with(input,order(mon,day)),]
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  if (batch){
    if ("id" %in% names(input)) {
      input<-input[with(input,order(mon,day,id)),]
      n <- length(unique(input$id))
      if(length(unique(input[1:n,"id"]))!=n){
        stop("Multiple stations have to start and end at the same dates,and input data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  }else{n <- nrow(input)}
  
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  mon<-input$mon
  dmc<-input$dmc
  if (!exists("dmc") | is.null(dmc)) 
    warning("dmc is missing!")
  if (!exists("temp") | is.null(temp)) 
    warning("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    warning("precipitation (prec) is missing!")
  if (!exists("ws") | is.null(ws)) 
    warning("wind speed (ws) is missing!")
  if (!exists("rh") | is.null(rh)) 
    warning("relative humidity (rh) is missing!")
#   if (!exists("mon") | is.null(mon)) 
#     warning("month (mon) is missing!")
  if (length(temp)%%n != 0)
    warning("Input data do not match with number of weather stations")
  
  el  <-c(6.5,7.5,9.0,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7.0,6.0)
  rh<-ifelse(rh>99.9,99.9,rh)
  rh<-ifelse(rh<0.0,10.0,rh)
  ws<-ifelse(ws<0.0,0.0,ws)
  prec<-ifelse(prec<0.0,0.0,prec)
  
  n0  <- length(temp) %/% n
  SDMC<-NULL
  for (i in 1:n0){
    k<-(n*(i-1)+1):(n*i)
    if (is.null(sdmc_old)){
      sdmc_old <- 2.6 + (1.7*dmc[k]) 
      sdmc_old <- sdmc_old-6.0
      sdmc_old <- ifelse(sdmc_old < 12,12,sdmc_old)
    } 
    t0<-ifelse(temp[k]< -1.1,-1.1,temp[k])     
    rk = 4.91/3.57*1.894*(t0+1.1)*(100-rh[k])*el[mon[k]]*0.0001   #this is a modification multpliper at front
    rw<-ifelse(prec[k]<7.69,0.218*prec[k]-0.094,0.83*prec[k]-4.8)     #this prec function is a modification 
    wmi<-20.0+280.0/exp(0.023*sdmc_old)
    b<-ifelse(sdmc_old<=33,100.0/(0.5+0.3*sdmc_old),14.0-1.3*log(sdmc_old))
    b<-ifelse(sdmc_old>65,6.2*log(sdmc_old)-17.2,b)
    wmr<-wmi+1000.0*rw/(48.77+b*rw)
    pr<-ifelse(prec[k]<=0.44,sdmc_old,43.43*(5.6348-log(wmr-20)))
    pr<-ifelse(pr<0,0,pr)
    SDMC0 <- pr + rk
    SDMC0<-ifelse(SDMC0<0,0,SDMC0)
    SDMC<-c(SDMC,SDMC0)
    sdmc_old<-SDMC0
  }
  SDMC
}  
