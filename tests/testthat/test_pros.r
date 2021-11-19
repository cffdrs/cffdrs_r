test_that("pros", {
  library(cffdrs)
  # manual single entry
  pros.in1 <- data.frame(t(c(2, -79.701027, 43.808872, 50, -79.699650, 43.808833
                             , 120, -79.700387, 43.809816)))
  colnames(pros.in1)<-c("T1", "LONG1", "LAT1", "T2", "LONG2", "LAT2", "T3", 
                        "LONG3", "LAT3")
  pros.out1 <- pros(pros.in1)
  # multiple entries using a dataframe
  # load the test dataframe for pros
  data("test_pros")
  pros(test_pros)
})
