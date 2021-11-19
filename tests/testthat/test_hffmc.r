test_that("hffmc", {
  library(cffdrs)
  data("test_hffmc")
  # show the data format:
  head(test_hffmc)
  # (1)hffmc default: 
  # Re-order the data by year, month, day, and hour:
  test_hffmc<-test_hffmc[with(test_hffmc, order(yr,mon,day,hr)),]
  # Because the test data has 24 hours input variables 
  # it is possible to calculate the hourly FFMC chronically 
  # through multiple days(with the default initial ffmc_old=85):
  test_hffmc$ffmc_default<-hffmc(test_hffmc) 
  # (2) Calculate FFMC for multiple stations:
  # Calculate hourly FFMC with only one initial 
  # value (ffmc_old=85), but multiple weather stations. 
  # Sort the input by date/time and the station id:
  test_hffmc<-test_hffmc[with(test_hffmc,order(yr,mon,hr)),]
  # Add weather station id:
  test_hffmc$id<-rep(1:10,nrow(test_hffmc)/10)
  #check the data:
  head(test_hffmc)
  test_hffmc$ffmc01<-hffmc(test_hffmc,batch=TRUE)
  # With multiple initial FFMC (ffmc_old) as a vector: 
  test_hffmc$ffmc02<- hffmc(test_hffmc,ffmc_old = sample(70:100,10, replace=TRUE),batch=TRUE)
  # One time step assuming all records are from different 
  # weather stations: 
  foo<-hffmc(test_hffmc,batch=FALSE)
  # (3) output all hourly FWI System variables:
  test_hffmc$id<-NULL
  test_hffmc<-test_hffmc[with(test_hffmc,    order(yr,mon,day,hr)),]
  foo<-hffmc(test_hffmc,hourlyFWI=TRUE)
  # this will not run: warning message requesting for daily BUI
  test_hffmc$bui<-100
  foo<-hffmc(test_hffmc,hourlyFWI=TRUE)
  # (4) Calculate time steps in case the time intervals are 
  # not uniform:
  dat0<-test_hffmc[sample(1:30,20),]
  dat0<-dat0[with(dat0,order(yr,mon,day,hr)),]
  # with or without calc.step, hffmc is going to generate
  # different FFMC values.
  # without calculating time step (default):
  hffmc(dat0,time.step=1)
  # with calc.step=TRUE, time.step=1 is applied to 
  # only the first record, the rests would be calculated:
  hffmc(dat0,time.step=1,calc.step=TRUE)
})
