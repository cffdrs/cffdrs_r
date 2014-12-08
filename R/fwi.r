fwi<-function(input,init=data.frame(ffmc=85,dmc=6,dc=15,lat=55),batch=TRUE,out="all",lat.adjust=TRUE,uppercase=TRUE)
{
  ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1,8.6, 8.1, 7.8)
  ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
  ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  names(input) <- tolower(names(input))
  
  if (is.vector(init)){init<-as.data.frame(t(init))}
  names(init) <- tolower(names(init))
  if (substr(names(init),1,1)[1]=="x"|substr(names(init),1,1)[1]=="v"){
    if (ncol(init) == 3){
      names(init) <- c("ffmc","dmc","dc")
      init$lat    <- 55
    }else if(ncol(init)==4){
      names(init) <- c("ffmc","dmc","dc","lat")
    }
  }
    
  ffmc_yda <- init$ffmc
  dmc_yda  <- init$dmc
  dc_yda   <- init$dc
 
  if ("lat" %in% names(input)) {
    lat <- input$lat
  }
  else {
    warning("latitude was not provided, assign default value 55")
    lat <- rep(55, nrow(input))
  }
  if ("long" %in% names(input)) {
    long <- input$long
  }
  else {
    warning("long was not provided, assign a default number -120")
    long <- rep(-120, nrow(input))
  }
  if ("yr" %in% names(input)) {
    yr <- input$yr
  }
  else {
    warning("Year was not provided, assigned default number 5000")
    yr <- rep(5000, nrow(input))
  }
  if ("mon" %in% names(input)) {
    mon <- input$mon
  }
  else {
    warning("Month was not provided, assigned the default value, July")
    mon <- rep(7, nrow(input))
  }
  if ("day" %in% names(input)) {
    day <- input$day
  }
  else {
    warning("day was not provided, assigned default number -99")
    day <- rep(-99, nrow(input))
  }

  if (batch){
    if ("id" %in% names(input)) {
      input<-input[with(input,order(yr,mon,day,id)),]
      n <- length(unique(input$id))
      if(length(unique(input[1:n,"id"]))!=n){
        stop("Multiple stations have to start and end at the same dates, and input data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  }else{n <- nrow(input)}

  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  if (!exists("temp") | is.null(temp)) 
    warning("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    warning("precipitation (prec) is missing!")
  if (!exists("ws") | is.null(ws)) 
    warning("wind speed (ws) is missing!")
  if (!exists("rh") | is.null(rh)) 
    warning("relative humidity (rh) is missing!")
  if (length(unique(!mon %in% 1:12))>1)
    warning("Month has to be between 1 and 12")
  if (length(unique(!day %in% 1:31))>1)
    warning("Day has to be between 1 and 31")
  if (length(unique(lat>90))>1|length(unique(lat< -90))>1)
    warning("Latitude has to be between -90 and 90")
  if (length(unique(long>180))>1|length(unique(long< -180))>1)
    warning("Longitude has to be between -180 and 180")


  if (length(temp)%%n != 0)
    warning("Missing records may generate wrong outputs")
  if (nrow(init)==1&n>1){
    warning("Same initial data were used for multiple weather stations")
    ffmc_yda<-rep(ffmc_yda,n)
    dmc_yda<-rep(dmc_yda,n)
    dc_yda<-rep(dc_yda,n)
  }
  if (nrow(init)>1&nrow(init)!=n)
    stop("Number of initial values do not match with number of weather stations")
  
  n0<-length(temp)/n
  ffmc<-dmc<-dc<-isi<-bui<-fwi<-dsr<-NULL
  for (i in 1:n0){
    k  <- ((i-1)*n+1):(i*n)
    
    rh[k] <- ifelse(rh[k] >= 100, 99.9999, rh[k])
    wmo <- 147.2 * (101 - ffmc_yda)/(59.5 + ffmc_yda)
    ra <- ifelse(prec[k] > 0.5, prec[k] - 0.5, prec[k])
    wmo <- ifelse(prec[k] > 0.5, ifelse(wmo > 150, wmo + 0.0015 * 
          (wmo - 150) * (wmo - 150) * sqrt(ra) + 42.5 * ra * exp(-100/(251 - 
           wmo)) * (1 - exp(-6.93/ra)), wmo + 42.5 * ra * exp(-100/(251 - wmo)) * (1 - exp(-6.93/ra))), wmo)
    wmo <- ifelse(wmo > 250, 250, wmo)
    ed <- 0.942 * (rh[k]^0.679) + (11 * exp((rh[k] - 100)/10)) + 0.18 * 
      (21.1 - temp[k]) * (1 - 1/exp(rh[k] * 0.115))
    ew <- 0.618 * (rh[k]^0.753) + (10 * exp((rh[k] - 100)/10)) + 0.18 * 
      (21.1 - temp[k]) * (1 - 1/exp(rh[k] * 0.115))
    z <- ifelse(wmo < ed & wmo < ew, 0.424 * (1 - (((100 - rh[k])/100)^1.7)) + 
         0.0694 * sqrt(ws[k]) * (1 - ((100 - rh[k])/100)^8), 0)
    x <- z * 0.581 * exp(0.0365 * temp[k])
    wm <- ifelse(wmo < ed & wmo < ew, ew - (ew - wmo)/(10^x), wmo)
    z <- ifelse(wmo > ed, 0.424 * (1 - (rh[k]/100)^1.7) + 0.0694 * sqrt(ws[k]) * (1 - (rh[k]/100)^8), z)
    x <- z * 0.581 * exp(0.0365 * temp[k])
    wm <- ifelse(wmo > ed, ed + (wmo - ed)/(10^x), wm)
    ffmc1 <- (59.5 * (250 - wm))/(147.2 + wm)
    ffmc1 <- ifelse(ffmc1 > 101, 101, ffmc1)
    ffmc1 <- ifelse(ffmc1 < 0, 0, ffmc1)
    t0 <- temp[k]
    t0 <- ifelse(t0 < (-1.1), -1.1, t0)
    rk <- 1.894 * (t0 + 1.1) * (100 - rh[k]) * ell01[mon[k]] * 1e-04
    if (lat.adjust) {
      rk <- ifelse(lat[k] <= 30 & lat[k] > 10, 1.894 * (t0 + 1.1) * (100 - rh[k]) * ell02[mon[k]] * 1e-04, rk)
      rk <- ifelse(lat[k] <= -10 & lat[k] > -30, 1.894 * (t0 + 1.1) * (100 - rh[k]) * ell03[mon[k]] * 1e-04, rk)
      rk <- ifelse(lat[k] <= -30 & lat[k] >= -90, 1.894 * (t0 + 1.1) * (100 - rh[k]) * ell04[mon[k]] * 1e-04, rk)
      rk <- ifelse(lat[k] <= 10 & lat[k] > -10, 1.894 * (t0 + 1.1) * (100 - rh[k]) * 9 * 1e-04, rk)
    }
    ra <- prec[k]
    rw <- 0.92 * ra - 1.27
    wmi <- 20 + 280/exp(0.023 * dmc_yda)
    b <- ifelse(dmc_yda <= 33, 100/(0.5 + 0.3 * dmc_yda), ifelse(dmc_yda <= 65, 14 - 1.3 * log(dmc_yda), 6.2 * log(dmc_yda) - 17.2))
    wmr <- wmi + 1000 * rw/(48.77 + b * rw)
    op <- options(warn = (-1))
    pr0 <- 43.43 * (5.6348 - log(wmr - 20))
    options(op)
    pr <- ifelse(prec[k] <= 1.5, dmc_yda, pr0)
    pr <- ifelse(pr < 0, 0, pr)
    dmc1 <- pr + rk
    dmc1 <- ifelse(dmc1 < 0, 0, dmc1)
    t0 <- ifelse(temp[k] < (-2.8), -2.8, t0)
    pe <- (0.36 * (t0 + 2.8) + fl01[mon[k]])/2
    if (lat.adjust) {
      pe <- ifelse(lat[k] <= -10, (0.36 * (t0 + 2.8) + fl02[mon[k]])/2, pe)
      pe <- ifelse(lat[k] > -10 & lat[k] <= 10, (0.36 * (t0 + 2.8) + 1.4)/2, pe)
    }
    ra <- prec[k]
    rw <- 0.83 * ra - 1.27
    smi <- 800 * exp(-1 * dc_yda/400)
    dr0 <- dc_yda - 400 * log(1 + 3.937 * rw/smi)
    dr0 <- ifelse(dr0 < 0, 0, dr0)
    dr <- ifelse(prec[k] <= 2.8, dc_yda, dr0)
    dc1 <- dr + pe
    dc1 <- ifelse(dc1 < 0, 0, dc1)
    fW <- exp(0.05039 * ws[k])
    fm <- 147.2 * (101 - ffmc1)/(59.5 + ffmc1)
    fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31)/49300000)
    isi1 <- 0.208 * fW * fF
    bui1 <- ifelse(dmc1 == 0 & dc1 == 0, 0, 0.8 * dc1 * dmc1/(dmc1 + 0.4 * dc1))
    p <- ifelse(dmc1 == 0, 0, (dmc1 - bui1)/dmc1)
    cc <- 0.92 + ((0.0114 * dmc1)^1.7)
    bui0 <- dmc1 - cc * p
    bui0 <- ifelse(bui0 < 0, 0, bui0)
    bui1 <- ifelse(bui1 < dmc1, bui0, bui1)
    bb <- ifelse(bui1 > 80, 0.1 * isi1 * (1000/(25 + 108.64/exp(0.023 * bui1))), 0.1 * isi1 * (0.626 * (bui1^0.809) + 2))
    fwi1 <- ifelse(bb <= 1, bb, exp(2.72 * ((0.434 * log(bb))^0.647)))
    dsr1 <- 0.0272 * (fwi1^1.77)
    ffmc<-c(ffmc,ffmc1)
    dmc<-c(dmc,dmc1)
    dc<-c(dc,dc1)
    isi<-c(isi,isi1)
    bui<-c(bui,bui1)
    fwi<-c(fwi,fwi1)
    dsr<-c(dsr,dsr1)
    ffmc_yda<-ffmc1
    dmc_yda<-dmc1
    dc_yda<-dc1
  } 
  
  if (out == "fwi") {
    new_FWI <- data.frame(ffmc=ffmc,dmc=dmc,dc=dc,isi=isi,bui=bui,fwi=fwi,dsr=dsr)
    if (uppercase){
      names(new_FWI)<-toupper(names(new_FWI))
    }
    new_FWI
  }
  else {
    if (out == "all") {
      new_FWI <- cbind(input,ffmc,dmc,dc,isi,bui,fwi,dsr)
      if (uppercase){
        names(new_FWI)<-toupper(names(new_FWI))
      }
      new_FWI
    }
  }
}
