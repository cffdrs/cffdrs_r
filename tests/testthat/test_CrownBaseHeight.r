test_that("CrownBaseHeight", {
  # fctCBH <-function(FUELTYPE, CBH, SD, SH)
  # {
  #   return(.CrownBaseHeight(FUELS[[FUELTYPE]], CBH, SD, SH))
  # }
  fctCBH <-function(FUELTYPE, CBH, SD, SH)
  {
    CBHs <- c(2, 3, 8, 4, 18, 7, 10, 0, 6, 6, 6, 6, 0, 0, 0,
              0, 0)
    names(CBHs) <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7",
                     "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A",
                     "O1B")
    CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH), ifelse(FUELTYPE %in%
                                                             c("C6") & SD > 0 & SH > 0, -11.2 + 1.06 * SH + 0.0017 *
                                                             SD, CBHs[FUELTYPE]), CBH)
    CBH <- ifelse(CBH < 0, 1e-07, CBH)
    return(CBH)
  }
  checkData('CrownBaseHeight',
            fctCBH,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(CBH=CBH),
                 data.table(SD=SD),
                 data.table(SH=SH)))
})
