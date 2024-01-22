test_that("TotalFuelConsumption", {
  checkData('TotalFuelConsumption',
            total_fuel_consumption,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CFL=CFL),
                 data.table(CFB=CFB),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(option="TFC")))
})
