test_that("lros", {
  library(cffdrs)
  # manual single entry, but converted to a data frame
  lros.in1 <- data.frame(t(c(0, 24.5, 50, 22.6, 120, 20.0, 90, 35)))
  colnames(lros.in1)<-c("T1","LengthT1T2", "T2", "LengthT1T3", "T3", 
                        "LengthT2T3", "bearingT1T2", "bearingT1T3")
  lros.out1 <- lros(lros.in1)
  lros.out1
  
  # multiple entries using a dataframe
  # load the test dataframe for lros
  data("test_lros")
  lros(test_lros)
})
