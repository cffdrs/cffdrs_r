test_that("CriticalSurfaceRateOfSpread", {
  fctCSI <- function(FUELTYPE, FMC, SFC, ROS, CBH, option)
  {
    CSI <- critical_surface_intensity(FMC, CBH)
    return(surface_fire_rate_of_spread(CSI, SFC))
  }
  checkData('CriticalSurfaceRateOfSpread',
            fctCSI,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(ROS=ROS),
                 data.table(CBH=CBH),
                 data.table(option=c("RSO"))))
})
