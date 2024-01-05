test_that("FireIntensity", {
  checkData('FireIntensity',
           FireIntensity,
           list(data.table(FC=FC),
                data.table(ROS=ROS)))
})
