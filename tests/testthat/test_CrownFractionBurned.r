test_that("CrownFractionBurned", {
  fctCFB <- function(FUELTYPE, FMC, SFC, ROS, CBH)
  {
    CSI <- critical_surface_intensity(FMC, CBH)
    RSO <- surface_fire_rate_of_spread(CSI, SFC)
    CFB <- crown_fraction_burned(ROS, RSO)
    return(CFB)
  }
  checkData('CrownFractionBurned',
            fctCFB,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(ROS=ROS),
                 data.table(CBH=CBH)))
})
