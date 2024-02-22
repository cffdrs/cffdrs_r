test_that("RateOfSpreadAtTime", {
  checkData('RateOfSpreadAtTime',
            rate_of_spread_at_time,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(ROSeq=ROS),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
