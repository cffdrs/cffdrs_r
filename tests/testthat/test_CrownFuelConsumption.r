test_that("CrownFuelConsumption", {
  fctCFC <- function(FUELTYPE, CFL, CFB, SFC, PC, PDF, option)
  {
    return(crown_fuel_consumption(FUELTYPE, CFL, CFB, PC, PDF))
  }
  checkData('CrownFuelConsumption',
            fctCFC,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CFL=CFL),
                 data.table(CFB=CFB),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(option="CFC")))
})
