test_that("CrownFuelConsumption", {
  fctTFC <- function(FUELTYPE, CFL, CFB, SFC, PC, PDF, option)
  {
    return(CrownFuelConsumption(FUELTYPE, CFL, CFB, PC, PDF))
  }
  checkData('CrownFuelConsumption',
            fctTFC,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CFL=CFL),
                 data.table(CFB=CFB),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(option="CFC")))
})
