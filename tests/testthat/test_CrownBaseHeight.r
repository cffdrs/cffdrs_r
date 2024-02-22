test_that("CrownBaseHeight", {
  checkData('CrownBaseHeight',
            crown_base_height,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CBH=CBH),
                 data.table(SD=SD),
                 data.table(SH=SH)))
})
