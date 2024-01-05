test_that("C6SurfaceRateOfSpread", {
  # fct <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  # {
  #   stopifnot("C6" == FUELTYPE)
  #   stopifnot("RSI" == option)
  #   RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
  #   return(SurfaceRateOfSpreadC6(RSI, BUI))
  # }
  fctRSSC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  {
    stopifnot("C6" == FUELTYPE)
    RSI <- .C6calc(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
    RSS <- RSI * .BEcalc(FUELTYPE, BUI)
    return(RSS)
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
