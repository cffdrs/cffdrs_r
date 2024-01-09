crown_fuel_load <- function(FUELTYPE, CFL) {
  # logic originally in fbp() pulled into its own function
    CFLs <- c(
    0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5,
    0, 0.8, 0.8, 0.8, 0.8, 0, 0, 0, 0, 0
  )
  names(CFLs) <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
  )
  CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), CFLs[FUELTYPE], CFL)
  return(CFL)
}