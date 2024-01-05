test_that("RateOfSpreadAtTheta", {
  checkData('RateOfSpreadAtTheta',
            rate_of_spread_at_theta,
            list(data.table(ROS=ROS),
                 data.table(FROS=ROS),
                 data.table(BROS=ROS),
                 data.table(THETA=THETA)))
})
