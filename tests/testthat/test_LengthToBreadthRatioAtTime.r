test_that("LengthToBreadthRatioAtTime", {
  checkData('LengthToBreadthRatioAtTime',
            LengthToBreadthRatioAtTime,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(LB=LB),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
