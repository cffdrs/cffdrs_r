hffmcRaster<-function(weatherstream,ffmc_old=85,time.step=1,hourlyFWI=FALSE) {
  names(weatherstream)<-tolower(names(weatherstream))
  Tp <- weatherstream$temp
  H  <- weatherstream$rh
  W  <- weatherstream$ws
  ro <- weatherstream$prec
  if (is.numeric(ffmc_old)){
    Fo <- Tp
    values(Fo)<-ffmc_old
  }else{Fo<-ffmc_old}
  if (!exists("Tp") | is.null(Tp)) 
    warning("temperature (temp) is missing!")
  if (!exists("ro") | is.null(ro)) 
    warning("precipitation (prec) is missing!")
  if (!exists("W") | is.null(W)) 
    warning("wind speed (ws) is missing!")
  if (!exists("H") | is.null(H)) 
    warning("relative humidity (rh) is missing!")

  mo <- 147.27723 * (101 - Fo)/(59.5 + Fo)
  
  mr1<-mo
  mr1[mr1>150]<-NA
  rf1<-mask(ro,mr1)
  mr1<-mr1 + 42.5 * rf1 * exp(-100/(251 - mr1)) * (1 - exp(-6.93/rf1))
  mr2<-mo
  mr2[mr2<=150]<-NA
  rf2<-mask(ro,mr2)
  mr2<-mr2 + 42.5 * rf2 * exp(-100/(251 - mr2)) *(1 - exp(-6.93/rf2)) + 0.0015 * ((mr2 - 150)^2) * (rf2^0.5)
  mr3<-cover(mr1,mr2)
  mr3[mr3>250]<-250
  
  r1<-ro
  r1[r1<=0]<-NA
  mr<-mask(mr3,r1)
  r1<-ro
  r1[r1>0]<-NA
  mo1<-mask(mo,r1)
  mo<-cover(mo1,mr)
  
  Ed <- 0.942 * (H^0.679) + 11 * exp((H - 100)/10) + 
      0.18 * (21.1 - Tp) * (1 - exp(-0.115 * H))
  ko <- 0.424 * (1 - (H/100)^1.7) + 0.0694 * 
      (W^0.5) * (1 - (H/100)^8)
  kd <- ko * 0.0579 * exp(0.0365 * Tp)
  md <- Ed + (mo - Ed) * (10^(-1*kd*time.step))
    
  Ew <- 0.618 * (H^0.753) + 10 * exp((H - 100)/10) + 
      0.18 * (21.1 - Tp) * (1 - exp(-0.115 * H))
  k1 <- 0.424 * (1 - ((100 - H)/100)^1.7) + 0.0694 * 
      (W^0.5) * (1 - ((100 - H)/100)^8)
  kw <- k1 * 0.0579 * exp(0.0365 * Tp)
  mw <- Ew - (Ew - mo) * (10^(-1*kw*time.step)) 
    
  m0<-overlay(mo,Ed,fun=function(a,b){return(a>b)})
  md[m0==0]<-NA
  mw[m0==1]<-NA
  m<-cover(md,mw)

  m1<-overlay(Ed,mo,Ew,fun=function(a,b,c)return(a>=b&b>=c))
  mo[m1==0]<-NA
  m[m1==1]<-NA
  m<-cover(mo,m)
  
  fo <- 59.5 * (250 - m)/(147.27723 + m)
  fo[fo<=0]<-0
  
  if (hourlyFWI){
    if ("bui" %in% names(weatherstream)){bui<-weatherstream$bui}
      
      fW <- exp(0.05039 * W)
      fm <- 147.2 * (101 - fo)/(59.5 + fo)
      fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31)/49300000)
      isi <- 0.208 * fW * fF
      
      bui1<-bui
      bui1[bui1<=80]<-NA
      bui1<-0.1 * isi * (1000/(25 + 108.64/exp(0.023 *bui1)))
      
      bui2<-bui
      bui2[bui1>80]<-NA
      bui2<-0.1 * isi * (0.626 * (bui2^0.809) + 2)
      bb<-cover(bui1,bui2)
      
      bb1<-bb
      bb1[bb>1]<-NA
      
      bb2<-bb
      bb2[bb<=1]<-NA
      bb2<-exp(2.72 * ((0.434 * log(bb2))^0.647))
      fwi<-cover(bb1,bb2)
      dsr <- 0.0272 * (fwi^1.77)
      output<-stack(fo,isi,fwi,dsr)
      names(output)<-c("hffmc","hisi","hfwi","hdsr")
      output
   } else {
    fo
  }
}

