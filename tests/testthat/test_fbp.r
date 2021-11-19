library(cffdrs)
  test_that("fbp", {
  # The dataset is the standard test data for FPB system
  # provided by Wotton et al (2009)
  data("test_fbp")
  head(test_fbp)
  #  id FuelType LAT LONG ELV FFMC BUI   WS WD GS  Dj  D0         hr PC PDF GFL cc theta Accel Aspect BUIEff CBH CFL ISI
  #1  1      C-1  55  110  NA   90 130 20.0  0 15 182  NA 0.33333333 NA  NA  NA  NA     0     1    270      1  NA  NA   0
  #2  2       C2  50   90  NA   97 119 20.4  0 75 121  NA 0.33333333 NA  NA  NA  NA     0     1    315      1  NA  NA   0
  #3  3      C-3  55  110  NA   95  30 50.0  0  0 182  NA 0.08333333 NA  NA  NA  NA     0     1    180      1  NA  NA   0
  #4  4      C-4  55  105 200   85  82  0.0 NA 75 182  NA 0.50000000 NA  NA  NA  NA     0     1    315      1  NA  NA   0
  #5  5       c5  55  105  NA   88  56  3.4  0 23 152 145 0.50000000 NA  NA  NA  NA     0     1    180      1  NA  NA   0
  
  #Primary output (default)
  fbp(test_fbp)
  #or
  fbp(test_fbp,output="Primary") 
  #or 
  fbp(test_fbp,"P")
  #Secondary output          
  fbp(test_fbp,"Secondary")
  #or
  fbp(test_fbp,"S")
  #All output          
  fbp(test_fbp,"All")
  #or
  fbp(test_fbp,"A")
  #For a single record:
  fbp(test_fbp[7,])  	
  #For a section of the records:
  fbp(test_fbp[8:13,])	
  #fbp function produces the default values if no data is fed to
  #the function:
  fbp()
})
