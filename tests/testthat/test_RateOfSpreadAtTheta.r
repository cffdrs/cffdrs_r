test_that("RateOfSpreadAtTheta", {
  name <- "RateOfSpreadAtTheta"
  fct <- rate_of_spread_at_theta
  checkResults(name,
               {
                 # HACK: already know how many warnings should be caused by the test data
                 suppressMessages(
                   expect_warning(
                     expect_warning(
                       expect_warning(
                         expect_warning(
                           { data <- makeDataFromInput(name, fct, ROS_AT_THETA_INPUT, split_args=TRUE, with_input=FALSE) },
                           "ROS must be >= FROS and BROS"),
                         "FROS must be > 0"),
                       "FROS must be > 0"),
                     "FROS must be > 0")
                   )
                 data
               }
             )
})
