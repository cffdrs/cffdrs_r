#' Crown Fraction Burned Calculator
#'
#' @description Calculate Calculate Crown Fraction Burned. To calculate CFB, we
#' also need to calculate Critical surface intensity (CSI), and Surface fire
#' rate of spread (RSO). The value of each of these equations can be returned
#' to the calling function without unecessary additional calculations.
#'
#' All variables names are laid out in the same manner as Forestry Canada Fire
#' Danger Group (FCFDG) (1992). Development and Structure of the Canadian
#' Forest Fire Behavior Prediction System." Technical Report ST-X-3, Forestry
#' Canada, Ottawa, Ontario.
#'
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068}
#' Development and Structure of the Canadian Forest Fire Behavior Prediction
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario.
#'
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param FMC      Foliar Moisture Content
#' @param SFC      Surface Fuel Consumption
#' @param CBH      Crown Base Height
#' @param ROS      Rate of Spread
#' @param option   Which variable to calculate ("ROS", "RSC", "RSI", or
#'                    "CFB" (the default).)
#'
#' @return CFB, CSI, RSO depending on which option was selected.
#' @noRd

critical_surface_intensity <- function(FMC, CBH)
{
  # FIX: .FuelNF returns non-NA values from this
  #Eq. 56 (FCFDG 1992) Critical surface intensity
  CSI <- 0.001 * (CBH**1.5) * (460 + 25.9 * FMC)**1.5
  return (CSI)
}

surface_fire_rate_of_spread <- function(CSI, SFC) {
  # Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
  # # no fuel consumption means no spread
  # RSO <- ifelse(0 == SFC,
  #               0,
  #               CSI / (300 * SFC))
  RSO <- CSI / (300 * SFC)
  return(RSO)
}

crown_fraction_burned <- function(ROS, RSO) {
  # Eq. 58 (FCFDG 1992) Crown fraction burned
  CFB <- ifelse(ROS > RSO, 1 - exp(-0.23 * (ROS - RSO)), 0)
  return(CFB)
}

.CFBcalc <- function(
    FUELTYPE, FMC, SFC, ROS, CBH,
    option = "CFB") {
  CSI <- critical_surface_intensity(FMC, CBH)
  # Return at this point, if specified by caller
  if (option == "CSI") {
    .Deprecated("critical_surface_intensity")
    return(CSI)
  }
  RSO <- surface_fire_rate_of_spread(CSI, SFC)
  # Return at this point, if specified by caller
  if (option == "RSO") {
    .Deprecated("surface_fire_rate_of_spread")
    return(RSO)
  }
  CFB <- crown_fraction_burned(ROS, RSO)
  .Deprecated("crown_fraction_burned")
  return(CFB)
}
