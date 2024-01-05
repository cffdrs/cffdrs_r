test_that("CrownFractionBurned", {
  fctCFB <- function(FUELTYPE, FMC, SFC, ROS, CBH)
  {
    CSI <- CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
    #Calculate Surface fire rate of spread (m/min)
    RSO <- CriticalSurfaceRateOfSpread(CSI, SFC)
    return(CrownFractionBurned(FUELTYPE, ROS, RSO))
  }
  checkData('CrownFractionBurned',
            fctCFB,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(ROS=ROS),
                 data.table(CBH=CBH)))
})
