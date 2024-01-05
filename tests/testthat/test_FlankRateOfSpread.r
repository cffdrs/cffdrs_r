test_that("FlankRateOfSpread", {
  checkData('FlankRateOfSpread',
            flank_rate_of_spread,
            list(data.table(ROS=ROS),
                 data.table(BROS=ROS),
                 data.table(LB=LB)))
})
