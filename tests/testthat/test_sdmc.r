test_that("sdmc", {
  library(cffdrs)
  data("test_sdmc")
  #order the data:
  test_sdmc<-test_sdmc[with(test_sdmc,order(yr,mon,day)),]
  # (1)Default of sdmc, calculate sdmc for a chronical period 
  # of time. 
  # Because sdmc_old is better to be calculated, we normally
  # ignore this option:
  test_sdmc$SDMC<-sdmc(test_sdmc)
  # (2) multiple weather stations: 
  # Batch process with multiple stations (2 stations) assuming
  # they are from the same month:
  test_sdmc$mon<-7
  test_sdmc$day<-rep(1:24,2)
  test_sdmc$id<-rep(1:2,each=24)
  # Sort the data by date and weather station id:
  test_sdmc<-test_sdmc[with(test_sdmc,order(yr,mon,day,id)),]
  # Apply the function
  test_sdmc$SDMC_mult_stn<-sdmc(test_sdmc,batch=TRUE)
  # Assuming each record is from a different weather station, and 
  # calculate only one time step: 
  foo<-sdmc(test_sdmc,batch=FALSE)
})
