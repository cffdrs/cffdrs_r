gfmc<-function(input,GFMCold=85,batch=TRUE,time.step=1,roFL=0.3,out="GFMCandMC"){
  t0<-time.step
  names(input) <- tolower(names(input))
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  isol<-input$isol

  if (!exists("temp") | is.null(temp)) 
    warning("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    warning("precipitation (prec) is missing!")
  if (!exists("ws") | is.null(ws)) 
    warning("wind speed (ws) is missing!")
  if (!exists("rh") | is.null(rh)) 
    warning("relative humidity (rh) is missing!")
  if (!exists("isol") | is.null(isol)) 
    warning("ISOL is missing!")  

  if (batch){
    if ("id" %in% names(input)) {
      n <- length(unique(input$id))
      if(length(unique(input[1:n,"id"]))!=n){
        stop("Multiple stations have to start and end at the same dates/time,and input data must be sorted by date/time and id")
      }
      
    } else {
      n <- 1
    }
  }else{n <- nrow(input)}
  
  if (length(temp)%%n != 0)
    warning("Input data do not match with number of weather stations")

  if (length(GFMCold)!=n&length(GFMCold)==1){
    warning("One GFMCold value for multiple weather stations") 
    GFMCold<-rep(GFMCold,n)
  }
  if (length(GFMCold)!=n&length(GFMCold)>1) stop("Number of GFMCold doesn't match number of wx stations")
  
  n0<-length(temp)%/%n
  GFMC<-NULL
  MC<-NULL
  for (i in 1:n0){
    k<-(n*(i-1)+1):(n*i)
    MCold<-147.2772*((101-GFMCold)/(59.5+GFMCold))
    MCr<-ifelse(prec[k]>0,MCold+100*(prec[k]/roFL),MCold)
    MCr<-ifelse(MCr>250,250,MCr)
    MCold<-MCr
    
    Tf<-temp[k]+35.07*isol[k]*exp(-0.06215*ws[k])
    eS.T<-6.107*10^(7.5*temp[k]/(237+temp[k]))
    eS.Tf<-6.107*10^(7.5*Tf/(237+Tf))
  
    RH.f<-rh[k]*(eS.T/eS.Tf)
  
    EMC.D<-(1.62*RH.f^0.532+13.7*exp((RH.f-100)/13.0))+0.27*(26.7-Tf)*(1-exp(-0.115*RH.f))
    EMC.W<-(1.42*RH.f^0.512+12.0*exp((RH.f-100)/18.0))+0.27*(26.7-Tf)*(1-exp(-0.115*RH.f))
  
    Rf<-ifelse(MCold>EMC.D,RH.f/100,rh)
    Rf<-ifelse(MCold<EMC.W,(100-RH.f)/100,Rf)
  
    K.GRASS<-0.389633*exp(0.0365*Tf)*(0.424*(1-Rf^1.7)+0.0694*sqrt(ws[k])*(1-Rf^8))

    MC0<-ifelse(MCold>EMC.D,EMC.D+(MCold-EMC.D)*exp(-1.0 * log(10.0)*K.GRASS*t0),MCold)
    MC0<-ifelse(MCold<EMC.W,EMC.W+(MCold-EMC.W)*exp(-1.0 * log(10.0)*K.GRASS*t0),MC0)

    GFMC0<-59.5*((250-MC0)/(147.2772+MC0))
    
    GFMC <- c(GFMC,GFMC0)
    MC <- c(MC, MC0)
    
    GFMCold <- GFMC0
    MCold <- MC0
  }
  if (out=="ALL"){
    All<-as.data.frame(cbind(input,GFMC,MC))
    All
  } else if (out=="GFMCandMC"){
    GFMCandMC <- data.frame(GFMC=GFMC,MC=MC)
    GFMCandMC
   } else if(out=="GFMC"){
    GFMC
   }
   else if (out=="MC"){
    MC
   }
}

