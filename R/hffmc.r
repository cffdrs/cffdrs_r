hffmc<-function(weatherstream,ffmc_old=85,time.step=1,calc.step=FALSE,batch=TRUE,hourlyFWI=FALSE) {
  t0 <- time.step
  names(weatherstream)<-tolower(names(weatherstream))
  if (batch){
    if ("id" %in% names(weatherstream)) { 
      n <- length(unique(weatherstream$id))
      if(length(unique(weatherstream[1:n,"id"]))!=n){
        stop("Multiple stations have to start and end at the same dates/time, and the data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  }else{n <- nrow(weatherstream)}
  
  if (length(ffmc_old)==1&n>1){Fo <- rep(ffmc_old,n)} else {Fo<-ffmc_old}
  
  hr <- weatherstream$hr
  Tp <- weatherstream$temp
  H  <- weatherstream$rh
  W  <- weatherstream$ws
  ro <- weatherstream$prec
  if (!exists("hr") | is.null(hr)) 
    warning("hour value is missing!")
  if (!exists("Tp") | is.null(Tp)) 
    warning("temperature (temp) is missing!")
  if (!exists("ro") | is.null(ro)) 
    warning("precipitation (prec) is missing!")
  if (!exists("W") | is.null(W)) 
    warning("wind speed (ws) is missing!")
  if (!exists("H") | is.null(H)) 
    warning("relative humidity (rh) is missing!")
  if (length(H)%%n != 0)
    warning("Weatherstream do not match with number of weather stations")
  
  n0<-length(H)/n
  f<-NULL
  for (i in 1:n0){
    k  <- ((i-1)*n+1):(i*n)
    if (calc.step&i > 1) {
      t0  <- ifelse(n0>1,hr[k]-hr[k-n],t0)
      t0  <- ifelse(t0 == -23,1,t0)
      t0  <- ifelse(t0<0,-1*t0,t0)
    }
    mo <- 147.27723 * (101 - Fo)/(59.5 + Fo)
    rf <- ro[k]
    mr <- ifelse(mo <= 150,mo + 42.5 * rf * exp(-100/(251 - mo)) * (1 - exp(-6.93/rf)),
                 mo + 42.5 * rf * exp(-100/(251 - mo)) *(1 - exp(-6.93/rf)) + 0.0015 * ((mo - 150)^2) * (rf^0.5))
    mr <- ifelse(mr > 250,250,mr)
    mo <- ifelse(ro[k] > 0.0,mr,mo)
    
    Ed <- 0.942 * (H[k]^0.679) + 11 * exp((H[k] - 100)/10) + 
      0.18 * (21.1 - Tp[k]) * (1 - exp(-0.115 * H[k]))
    ko <- 0.424 * (1 - (H[k]/100)^1.7) + 0.0694 * 
      (W[k]^0.5) * (1 - (H[k]/100)^8)
    kd <- ko * 0.0579 * exp(0.0365 * Tp[k])
    md <- Ed + (mo - Ed) * (10^(-kd*t0))
    
    Ew <- 0.618 * (H[k]^0.753) + 10 * exp((H[k] - 100)/10) + 
      0.18 * (21.1 - Tp[k]) * (1 - exp(-0.115 * H[k]))
    k1 <- 0.424 * (1 - ((100 - H[k])/100)^1.7) + 0.0694 * 
      (W[k]^0.5) * (1 - ((100 - H[k])/100)^8)
    kw <- k1 * 0.0579 * exp(0.0365 * Tp[k])
    mw <- Ew - (Ew - mo) * (10^(-kw*t0)) 
    
    m  <- ifelse(mo > Ed,md,mw)
    m  <- ifelse(Ed >= mo & mo >= Ew,mo,m)
    Fo <- 59.5 * (250 - m)/(147.27723 + m)
    Fo <- ifelse(Fo <=0,0,Fo)
    f<-c(f,Fo)
  }
  if (hourlyFWI){
    bui<-weatherstream$bui
    if (!exists("bui") | is.null(bui)){ 
      warning("Daily BUI is required to calculate hourly FWI")
    }else{
      
      fW <- exp(0.05039 * W)
      fm <- 147.2 * (101 - f)/(59.5 + f)
      fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31)/49300000)
      isi <- 0.208 * fW * fF
      
      bb <- ifelse(bui > 80, 0.1 * isi * (1000/(25 + 108.64/exp(0.023 *bui))), 
                   0.1 * isi * (0.626 * (bui^0.809) + 2))
      fwi <- ifelse(bb <= 1, bb, exp(2.72 * ((0.434 * log(bb))^0.647)))
      dsr <- 0.0272 * (fwi^1.77)
      output<-cbind(weatherstream,data.frame(ffmc=f,isi=isi,fwi=fwi,dsr=dsr))
      output
    }
  } else {
    f
  }
}
