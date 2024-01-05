fctSlopeAdjust <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                           PC, PDF, CC, CBH, ISI, output)
{
  result <- SlopeAdjust(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
                        FMC, SFC, PC, PDF, CC, CBH, ISI)
  if ('WSV' == output)
  {
    return(result[["WSV"]])
  }
  return(result[["RAZ"]])
  # return(result[[output]])
}
test_that("SlopeAdjust$RAZ", {
  checkData('SlopeAdjustRAZ',
            fctSlopeAdjust,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FFMC=FFMC),
                 data.table(BUI=BUI),
                 data.table(WS=WS),
                 data.table(WAZ=WAZ),
                 data.table(GS=GS),
                 data.table(SAZ=SAZ),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(CC=CC),
                 data.table(CBH=CBH),
                 data.table(ISI=ISI),
                 data.table(output = "RAZ")))
})
test_that("SlopeAdjust$WSV", {
  checkData('SlopeAdjustWSV',
            fctSlopeAdjust,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FFMC=FFMC),
                 data.table(BUI=BUI),
                 data.table(WS=WS),
                 data.table(WAZ=WAZ),
                 data.table(GS=GS),
                 data.table(SAZ=SAZ),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(CC=CC),
                 data.table(CBH=CBH),
                 data.table(ISI=ISI),
                 data.table(output = "WSV")))
})
