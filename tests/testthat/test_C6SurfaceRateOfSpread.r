test_that("C6SurfaceRateOfSpread", {
  fctRSSC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  {
    stopifnot("C6" == FUELTYPE)
    stopifnot("RSI" == option)
    RSI <- intermediate_surface_rate_of_spread_c6(ISI)
    return(surface_rate_of_spread_c6(RSI, BUI))
  }
  checkData('C6SurfaceRateOfSpread',
            fctRSSC6,
            list(data.table(FUELTYPE=c("C6")),
                 data.table(ISI=ISI),
                 data.table(BUI=BUI),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(CBH=CBH),
                 data.table(ROS=ROS),
                 data.table(CFB=CFB),
                 data.table(RSC=ROS),
                 data.table(option=c("RSI"))))
})
