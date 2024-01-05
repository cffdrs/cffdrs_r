test_that("TotalFuelConsumption", {
  fctTFC <- function(FUELTYPE, CFL, CFB, SFC, PC, PDF, option)
  {
    CFC <- CrownFuelConsumption(FUELTYPE, CFL, CFB, PC, PDF)
    return(TotalFuelConsumption(CFC, SFC))
  }
  checkData('TotalFuelConsumption',
            fctTFC,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CFL=CFL),
                 data.table(CFB=CFB),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(option="TFC")))
})
  