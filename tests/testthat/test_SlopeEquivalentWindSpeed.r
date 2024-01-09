test_that("SlopeEquivalentWindSpeed", {
  fctSlopeWSE <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    return(.Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                         PC, PDF, CC, CBH, ISI, "WSE"))
  }
  fctSlopeWSX <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    return(.Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                         PC, PDF, CC, CBH, ISI, "WSX"))
  }
  fctSlopeWSY <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    return(.Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                         PC, PDF, CC, CBH, ISI, "WSY"))
  }
  fctSlopeWSV <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    return(.Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                         PC, PDF, CC, CBH, ISI, "WSV"))
  }
  fctSlopeRAZ <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    return(.Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC,
                         PC, PDF, CC, CBH, ISI, output))
  }
  # HACK: use extra variables so we generate same rows as SlopeAdjust test
  checkData('SlopeEquivalentWindSpeed',
            fctSlopeWSE,
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
  checkData('SlopeAdjustWSX',
            fctSlopeWSX,
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
  checkData('SlopeAdjustWSY',
            fctSlopeWSY,
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
  checkData('SlopeAdjust_WSV',
            fctSlopeWSV,
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
  checkData('SlopeAdjust_RAZ',
            fctSlopeRAZ,
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
