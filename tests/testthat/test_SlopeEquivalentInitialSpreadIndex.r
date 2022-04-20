test_that("SlopeEquivalentInitialSpreadIndex", {
  fctSlopeISI <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                             PC, PDF, CC, CBH, ISI, output)
  {
    result <- .SlopeEquivalentInitialSpreadIndex(FUELS[[FUELTYPE]], FFMC, BUI,
                                                 WS, WAZ, GS, SAZ, FMC, SFC,
                                                 PC, PDF, CC, CBH, ISI)
    return(result)
  }
  # HACK: use extra variables so we generate same rows as SlopeAdjust test
  checkData('SlopeEquivalentInitialSpreadIndex',
            fctSlopeISI,
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
