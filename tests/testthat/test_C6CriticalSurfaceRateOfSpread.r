test_that("C6CriticalSurfaceRateOfSpread", {
  fct <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  {
    stopifnot("C6" == FUELTYPE)
    stopifnot("CFB" == option)
    RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
    RSS <- SurfaceRateOfSpreadC6(RSI, BUI)
    RSC <- CrownRateOfSpreadC6(ISI, FMC)
    CSI <- CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
    #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
    RSO <- CSI / (300 * SFC)
    return(RSO)
  }
  checkData('C6CriticalSurfaceRateOfSpread',
            fct,
            list(data.table(FUELTYPE=c("C6")),
                 data.table(ISI=ISI),
                 data.table(BUI=BUI),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(CBH=CBH),
                 data.table(ROS=ROS),
                 data.table(CFB=CFB),
                 data.table(RSC=ROS),
                 data.table(option=c("CFB"))))
})
