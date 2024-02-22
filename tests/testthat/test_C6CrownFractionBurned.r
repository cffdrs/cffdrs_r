test_that("C6CrownFractionBurned", {
  fct <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  {
    stopifnot("C6" == FUELTYPE)
    stopifnot("CFB" == option)
    RSI <- intermediate_surface_rate_of_spread_c6(ISI)
    RSC <- crown_rate_of_spread_c6(ISI, FMC)
    RSS <- surface_rate_of_spread_c6(RSI, BUI)
    CSI <- critical_surface_intensity(FMC, CBH)
    RSO <- surface_fire_rate_of_spread(CSI, SFC)
    CFB <- ifelse((RSC > RSS) && (RSS > RSO), crown_fraction_burned(RSS, RSO), 0)
    return(CFB)
  }
  checkData('C6CrownFractionBurned',
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
