test_that("RateOfSpreadAtTheta", {
  checkData('RateOfSpreadAtTheta',
            RateOfSpreadAtTheta,
            list(data.table(ROS=ROS),
                 data.table(FROS=ROS),
                 data.table(BROS=ROS),
                 data.table(THETA=THETA)))
})
