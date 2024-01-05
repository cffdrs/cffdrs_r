# test_that("hffmcRaster", {
#   library(cffdrs)
#   require(raster)
#   ## load the test data for the first hour, namely hour01:
#   hour01src <- system.file("extdata","test_rast_hour01.tif",package="cffdrs")
#   hour01 <- stack(hour01src)
#   # Assign names to the layers:
#   names(hour01)<-c("temp","rh","ws","prec")
#   # (1) Default, based on the initial value: 
#   foo<-hffmcRaster(hour01)
#   plot(foo)
#   ### Additional, longer running examples ###
#   # (2) Based on previous day's hffmc:
#   # load the test data for the second hour, namely hour02:
#   hour02src <- system.file("extdata","test_rast_hour02.tif",package="cffdrs")
#   hour02 <- stack(hour02src)
#   # Assign variable names to the layers:
#   names(hour02)<-c("temp","rh","ws","prec")
#   foo1<-hffmcRaster(hour02,ffmc_old=foo)
#   plot(foo1)
#   # (3) Calculate other hourly FWI components (ISI, FWI, and DSR):
#   # Need BUI layer, 
#   bui<-hour02$temp
#   values(bui)<-50
#   hour02<-stack(hour02,bui)
#   # Re-assign variable names to the layers:
#   names(hour02)<-c("temp","rh","ws","prec","bui")
#   # Calculate all the variables:
#   foo2<-hffmcRaster(hour02,ffmc_old=foo,hourlyFWI=TRUE)
#   # Visualize the maps:
#   plot(foo2)
# })
