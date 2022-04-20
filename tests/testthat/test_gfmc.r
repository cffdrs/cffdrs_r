# test_that("gfmc", {
#   library(cffdrs)
#   #load the test data
#   data("test_gfmc")
#   # show the data format:
#   head(test_gfmc)
#   #     yr mon day hr temp   rh   ws prec  isol
#   # 1 2006   5  17 10 15.8 54.6  5.0    0 0.340
#   # 2 2006   5  17 11 16.3 52.9  5.0    0 0.380
#   # 3 2006   5  17 12 18.8 45.1  5.0    0 0.626
#   # 4 2006   5  17 13 20.4 40.8  9.5    0 0.656
#   # 5 2006   5  17 14 20.1 41.7  8.7    0 0.657
#   # 6 2006   5  17 15 18.6 45.8 13.5    0 0.629
#   # (1) gfmc default: 
#   # Re-order the data by year, month, day, and hour:
#   dat<-test_gfmc[with(test_gfmc,order(yr,mon,day,hr)),]
#   # Because the test data has 24 hours input variables 
#   # it is possible to calculate the hourly GFMC continuously 
#   # through multiple days(with the default initial GFMCold=85):
#   dat$gfmc_default<-gfmc(dat) 
#   # two variables will be added to the input, GFMC and MC
#   head(dat)
#   # (2) For multiple weather stations:
#   # One time step (1 hour) with default initial value:
#   foo<-gfmc(dat,batch=FALSE)
#   # Chronical hourly GFMC with only one initial 
#   # value (GFMCold=85), but multiple weather stations. 
#   # Note: data is ordered by date/time and the station id. Subset 
#   # the data by keeping only the first 10 hours of observations 
#   # each day:
#   dat1<-subset(dat,hr\%in\%c(0:9))
#   #assuming observations were from the same day but with 
#   #9 different weather stations:
#   dat1$day<-NULL
#   dat1<-dat1[with(dat1,order(yr,mon,hr)),]
#   dat1$id<-rep(1:8,nrow(dat1)/8)
#   #check the data:
#   head(dat1)
#   # Calculate GFMC for multiple stations:
#   dat1$gfmc01<-gfmc(dat1,batch=TRUE)
#   # We can provide multiple initial GFMC (GFMCold) as a vector:   
#   dat1$gfmc02<- gfmc(dat1,GFMCold = sample(70:100,8, replace=TRUE),batch=TRUE)
#   # (3)output argument
#   ## include all inputs and outputs:
#   dat0<-dat[with(dat,order(yr,mon,day,hr)),]
#   foo<-gfmc(dat,out="ALL")
#   ## subhourly time step:
#   gfmc(dat0,time.step=1.5)
# })
