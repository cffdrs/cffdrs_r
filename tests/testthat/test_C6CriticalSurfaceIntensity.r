test_that("C6CriticalSurfaceIntensity", {
  fctCSIC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  {
    stopifnot("C6" == FUELTYPE)
    stopifnot("RSC" == option)
    # CSI <- CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
    # return(CSI)
    CSI <- cffdrs:::.CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, "CSI")
    return(CSI)
  }
  checkData('C6CriticalSurfaceIntensity',
            fctCSIC6,
            list(data.table(FUELTYPE=c("C6")),
                 data.table(ISI=ISI),
                 data.table(BUI=BUI),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(CBH=CBH),
                 data.table(ROS=ROS),
                 data.table(CFB=CFB),
                 data.table(RSC=ROS),
                 data.table(option=c("RSC"))))
})
