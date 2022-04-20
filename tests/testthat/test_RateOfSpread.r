test_that("RateOfSpread", {
  checkData('RateOfSpread',
            RateOfSpread,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(ISI=ISI),
                 data.table(BUI=BUI),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(CC=CC),
                 data.table(CBH=CBH)))
})
