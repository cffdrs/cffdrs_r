test_that("RateOfSpreadAtTime", {
  checkData('RateOfSpreadAtTime',
            RateOfSpreadAtTime,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(ROSeq=ROS),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
