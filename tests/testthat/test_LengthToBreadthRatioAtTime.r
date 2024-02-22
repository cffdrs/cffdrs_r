test_that("LengthToBreadthRatioAtTime", {
  checkData('LengthToBreadthRatioAtTime',
            length_to_breadth_at_time,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(LB=LB),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
