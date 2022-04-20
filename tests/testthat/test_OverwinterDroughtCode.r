test_that("OverwinterDroughtCode_test_wDC_1", {
  checkResults('OverwinterDroughtCode_test_wDC_1',
               OverwinterDroughtCode(DCf=300,rw=110))
})
test_that("OverwinterDroughtCode_test_wDC_2", {
  checkResults('OverwinterDroughtCode_test_wDC_2',
               OverwinterDroughtCode(DCf=300,rw=110,a=1.0,b=0.9))
})
test_that("OverwinterDroughtCode_test_wDC_3", {
  checkResults('OverwinterDroughtCode_test_wDC_3',
               OverwinterDroughtCode(DCf=c(400,300,250), rw=c(99,110,200),
                                     a=c(0.75,1.0,0.75), b=c(0.75,0.9,0.75)))
})
test_that("OverwinterDroughtCode_test_wDC_4", {
  test_wDC <- read.csv2('../../data/test_wDC.csv', sep=';')
  test_wDC$prec <- as.numeric(test_wDC$prec)
  input <- test_wDC
  input <- with(input,input[order(id,yr,mon,day),])
  input$date <- as.Date(as.POSIXlt(paste(input$yr,"-",input$mon,"-",input$day,sep="")))
  #select id value 1
  input.2 <- input[input$id==2,]
  test_wDC_fs <- read.csv2('../../data/test_wDC_fs.csv', sep=';')
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
  checkResults('OverwinterDroughtCode_test_wDc_4',
               OverwinterDroughtCode(DCf=fallDC,rw=curYr.prec))
})
test_that("OverwinterDroughtCode_test_wDC_5", {
  test_wDC <- read.csv2('../../data/test_wDC.csv', sep=';')
  test_wDC$prec <- as.numeric(test_wDC$prec)
  input <- test_wDC
  input <- with(input,input[order(id,yr,mon,day),])
  input$date <- as.Date(as.POSIXlt(paste(input$yr,"-",input$mon,"-",input$day,sep="")))
  #select id value 1
  input.2 <- input[input$id==2,]
  test_wDC_fs <- read.csv2('../../data/test_wDC_fs.csv', sep=';')
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
  fallDC <- 250
  checkResults('OverwinterDroughtCode_test_wDc_5',
               OverwinterDroughtCode(DCf=fallDC,rw=curYr.prec))
})
test_that("OverwinterDroughtCode", {
  checkData('OverwinterDroughtCode',
            OverwinterDroughtCode,
            list(data.table(DCf=DMC),
                 data.table(rw=seq(0, 1000)),
                 data.table(a=FRACTION),
                 data.table(b=FRACTION)))
})
