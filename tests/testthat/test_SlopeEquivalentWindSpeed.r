test_that("SlopeEquivalentWindSpeed", {
  fctSlopeWSE <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output = "RAZ")
  {
    ISF <- .SlopeEquivalentInitialSpreadIndex(FUELS[[FUELTYPE]], FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
    if (is.na(ISF) || -99.0 == ISF)
    {
      return(NA)
    }
    #Eq. 46 (FCFDG 1992)
    m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
    #Eq. 45 (FCFDG 1992) - FFMC function from the ISI equation
    fF <- 91.9 * exp(-.1386 * m) * (1 + (m**5.31) / 4.93e7)
    #Eqs. 44a, 44d (Wotton 2009) - Slope equivalent wind speed
    WSE <- 1 / 0.05039 * log(ISF / (0.208 * fF))
    #Eqs. 44b, 44e (Wotton 2009) - Slope equivalent wind speed
    WSE <- ifelse(WSE > 40 & ISF < (0.999 * 2.496 * fF),
                  28 - (1 / 0.0818 * log(1 - ISF/ ( 2.496 * fF))),
                  WSE)
    #Eqs. 44c (Wotton 2009) - Slope equivalent wind speed
    WSE <- ifelse(WSE > 40 & ISF >= (0.999 * 2.496 * fF), 112.45, WSE)
    return(WSE)
  }
  fctSlopeWSX <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output = "RAZ")
  {
    WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                       CC, CBH, ISI, output)
    #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
    WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
    return(WSX)
  }
  fctSlopeWSY <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                       CC, CBH, ISI, output)
    #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
    WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
    return(WSY)
  }
  fctSlopeWSV <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                       CC, CBH, ISI, output)
    #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
    WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
    #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
    WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
    #Eq. 49 (FCFDG 1992) - the net effective wind speed
    WSV <- sqrt(WSX * WSX + WSY * WSY)
    return(WSV)
  }
  fctSlopeRAZ <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output)
  {
    WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                       CC, CBH, ISI, output)
    #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
    WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
    #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
    WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
    #Eq. 49 (FCFDG 1992) - the net effective wind speed
    WSV <- sqrt(WSX * WSX + WSY * WSY)
    #Eq. 50 (FCFDG 1992) - the net effective wind direction (radians)
    RAZ <- acos(WSY / WSV)
    #Eq. 51 (FCFDG 1992) - convert possible negative RAZ into more understandable
    # directions
    RAZ <- ifelse(WSX < 0, 2 * pi - RAZ, RAZ)
    return(RAZ)
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
