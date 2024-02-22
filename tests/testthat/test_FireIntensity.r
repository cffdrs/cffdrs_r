test_that("FireIntensity", {
  checkData('FireIntensity',
           fire_intensity,
           list(data.table(FC=FC),
                data.table(ROS=ROS)))
})
