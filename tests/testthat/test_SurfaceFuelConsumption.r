test_that("SurfaceFuelConsumption", {
  checkData('SurfaceFuelConsumption',
            surface_fuel_consumption,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FFMC=FFMC),
                 data.table(BUI=BUI),
                 data.table(PC=PC),
                 data.table(GFL=GFL)))})
