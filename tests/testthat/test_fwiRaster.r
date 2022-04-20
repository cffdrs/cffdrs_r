# test_that("fwiRastwer", {
#   library(cffdrs)
#   require(raster)
#   # The test data is a stack with four input variables including 
#   # daily noon temp, rh, ws, and prec (we recommend tif format):
#   day01src <- system.file("extdata","test_rast_day01.tif",package="cffdrs")
#   day01 <- stack(day01src)
#   day01 <- crop(day01,c(250,255,47,51))
#   # assign variable names:
#   names(day01)<-c("temp","rh","ws","prec")
#   # (1) use the initial values
#   foo<-fwiRaster(day01)
#   plot(foo)
#   ### Additional, longer running examples ###
#   # (2) use initial values with larger raster
#   day01 <- stack(day01src)
#   names(day01)<-c("temp","rh","ws","prec")
#   foo<-fwiRaster(day01)
#   plot(foo)
# })
