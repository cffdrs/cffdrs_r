# test_that("fwi", {
#   # The test data is a standard test
#   # dataset for FWI system (Van Wagner and Pickett 1985) 
#   data("test_fwi")
#   # Show the data, which is already sorted by time:
#   # head(test_fwi)
#   # long  lat	yr	mon	day	temp	rh	ws	prec
#   # -100	40	1985	4	  13	17	  42	25	0
#   # -100	40	1985	4	  14	20	  21	25	2.4
#   # -100	40	1985	4	  15	8.5	  40	17	0
#   # -100	40	1985	4	  16	6.5	  25	6	0
#   # -100	40	1985	4	  17	13	  34	24	0
#   
#   ## (1) FWI System variables for a single weather station:
#   # Using the default initial values and batch argument, 
#   # the function calculate FWI variables chronically:
#   fwi.out1<-fwi(test_fwi) 				
#   # Using a different set of initial values:
#   fwi.out2<-fwi(test_fwi,init=data.frame(ffmc=80, dmc=10,dc=16, lat=50))
#   # This could also be done as the following:
#   fwi.out2<-fwi(test_fwi,init=data.frame(80,10,6,50))
#   # Or:
#   fwi.out2<-fwi(test_fwi,init=c(80,10,6,50))
#   # Latitude could be ignored, and the default value (55) will 
#   # be used:
#   fwi.out2<-fwi(test_fwi,init=data.frame(80,10,6))
#   
#   ## (2) FWI for one or multiple stations in a single day:
#   # Change batch argument to FALSE, fwi calculates FWI 
#   # components based on previous day's fwi outputs:
#   
#   fwi.out3<-fwi(test_fwi,init=fwi.out1,batch=FALSE)                 
#   # Using a suite of initials, assuming variables from fwi.out1
#   # are the initial values for different records. 
#   init_suite<-fwi.out1[,c("FFMC","DMC","DC","LAT")]
#   # Calculating FWI variables for one day but with multiple
#   # stations. Because the calculations is for one time step, 
#   # batch=FALSE:
#   fwi.out4<-fwi(test_fwi,init=init_suite,batch=FALSE)
#   
#   ## (3) FWI for multiple weather stations over a period of time: 
#   #Assuming there are 4 weather stations in the test dataset, and they are 
#   # ordered by day:
#   test_fwi$day<-rep(1:(nrow(test_fwi)/4),each=4)
#   test_fwi$id<-rep(1:4,length(unique(test_fwi$day)))
#   # Running the function with the same default initial inputs, will receive a 
#   # warning message, but that is fine: 
#   fwi(test_fwi)
#   
#   ## (4) Daylength adjustment:
#   # Change latitude values where the monthly daylength adjustments
#   # are different from the standard ones
#   test_fwi$lat<-22
#   # With daylength adjustment
#   fwi(test_fwi)[1:3,]
#   # Without daylength adjustment
#   fwi(test_fwi,lat.adjust=FALSE)[1:3,]
# })
