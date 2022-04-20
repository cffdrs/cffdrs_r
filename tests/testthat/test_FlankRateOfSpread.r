test_that("FlankRateOfSpread", {
  checkData('FlankRateOfSpread',
            FlankRateOfSpread,
            list(data.table(ROS=ROS),
                 data.table(BROS=ROS),
                 data.table(LB=LB)))
})
