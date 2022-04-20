test_that("CrownBaseHeight", {
  fctCBH <-function(FUELTYPE, CBH, SD, SH)
  {
    return(.CrownBaseHeight(FUELS[[FUELTYPE]], CBH, SD, SH))
  }
  checkData('CrownBaseHeight',
            fctCBH,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CBH=CBH),
                 data.table(SD=SD),
                 data.table(SH=SH)))
})
