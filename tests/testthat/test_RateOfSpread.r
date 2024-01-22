test_that("RateOfSpread", {
  checkData('RateOfSpread',
            rate_of_spread,
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
