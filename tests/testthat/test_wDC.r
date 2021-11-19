test_that("winter_DC", {
  
  library(cffdrs)
  # The standard test data:
  data("test_wDC")
  # (1) Simple case previous fall's DC was 300, overwinter 
  # rain 110mm
  winter_DC <- wDC(DCf=300,rw=110)
  winter_DC
  #(2) modified a and b parameters. Find table values in listed 
  # reference for Lawson and Armitage, 2008.
  winter_DC <- wDC(DCf=300,rw=110,a=1.0,b=0.9)
  winter_DC
  #(3)with multiple inputs:
  winter_DC <- wDC(DCf=c(400,300,250), rw=c(99,110,200),
                   a=c(0.75,1.0,0.75), b=c(0.75,0.9,0.75))
  winter_DC
  #(4) A realistic example:
  #precipitation accumulation and date boundaries
  input <- test_wDC
  #order data by ID and date
  input <- with(input,input[order(id,yr,mon,day),])
  input$date <- as.Date(as.POSIXlt(paste(input$yr,"-",input$mon,"-",input$day,sep="")))
  #select id value 1
  input.2 <- input[input$id==2,]
  #Explicitly defined fire start and end dates.
  data("test_wDC_fs")
  print(test_wDC_fs)
  #Set date field
  test_wDC_fs$date <- as.Date(as.POSIXlt(paste(test_wDC_fs$yr,"-",test_wDC_fs$mon,"-",
                                               test_wDC_fs$day,sep="")))
  #match to current id value
  input.2.fs <- test_wDC_fs[test_wDC_fs$id==2,]
  #assign start of winter date (or end of fire season date)
  winterStartDate <- input.2.fs[2,"date"]
  #assign end of winter date (or start of new fire season date)
  winterEndDate <-  input.2.fs[3,"date"]
  #Accumulate overwinter precip based on chosen dates
  curYr.prec <- sum(input.2[(input.2$date>winterStartDate & input.2$date < winterEndDate),]$prec)
  #Assign a fall DC value
  fallDC <- 500
  #calculate winter DC
  winter_DC <- wDC(DCf=fallDC,rw=curYr.prec)
  winter_DC
  #Assign a different fall DC value
  fallDC <- 250
  #calculate winter DC
  winter_DC <- wDC(DCf=fallDC,rw=curYr.prec,a=1.0)
  winter_DC
})
