test_that("DistanceAtTime", {
  checkData('DistanceAtTime',
            distance_at_time,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(ROSeq=ROS),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
