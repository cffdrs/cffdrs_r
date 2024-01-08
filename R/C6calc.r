#' C-6 Conifer Plantaion Fire Spread Calculator
#'
#' @description Calculate c6 (Conifer plantation) Fire Spread. C6 is a special
#' case, and thus has it's own function. To calculate C6 fire spread, this
#' function also calculates and can return ROS, CFB, RSC, or RSI by specifying
#' in the option parameter.
#' All variables names are laid out in the same manner as Forestry Canada Fire
#' Danger Group (FCFDG) (1992). Development and Structure of the Canadian
#' Forest Fire Behavior Prediction System." Technical Report ST-X-3, Forestry
#' Canada, Ottawa, Ontario.
#'
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068}
#' Development and Structure of the Canadian Forest Fire Behavior Prediction
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario.
#'
#' @param FUELTYPE    The Fire Behaviour Prediction FuelType
#' @param ISI         Initial Spread Index
#' @param BUI         Buildup Index
#' @param FMC         Foliar Moisture Content
#' @param SFC         Surface Fuel Consumption
#' @param CBH         Crown Base Height
#' @param ROS         Rate of Spread
#' @param CFB         Crown Fraction Burned
#' @param RSC         Crown Fire Spread Rate (m/min)
#' @param option      Which variable to calculate ("ROS", "RSC", "RSI", or
#'                      "CFB" (the default).)
#'
#' @return ROS, CFB, RSC or RSI depending on which option was selected
#' @noRd

intermediate_surface_rate_of_spread_c6 <- function(ISI) {
  # Eq. 62 (FCFDG 1992) Intermediate surface fire spread rate
  RSI <- 30 * (1 - exp(-0.08 * ISI))**3.0
  return(RSI)
}

surface_rate_of_spread_c6 <- function(RSI, BUI)
{
  # Eq. 63 (FCFDG 1992) Surface fire spread rate (m/min)
  RSS <- RSI * buildup_effect("C6", BUI)
  return (RSS)
}

crown_rate_of_spread_c6 <- function(ISI, FMC)
{
  #Average foliar moisture effect
  FMEavg <- 0.778
  #Eq. 59 (FCFDG 1992) Crown flame temperature (degrees K)
  tt <- 1500 - 2.75 * FMC
  #Eq. 60 (FCFDG 1992) Head of ignition (kJ/kg)
  H <- 460 + 25.9 * FMC
  #Eq. 61 (FCFDG 1992) Average foliar moisture effect
  FME <- ((1.5 - 0.00275 * FMC)**4.)/(460 + 25.9 * FMC) * 1000
  #Eq. 64 (FCFDG 1992) Crown fire spread rate (m/min)
  RSC <- 60 * (1 - exp(-0.0497 * ISI)) * FME / FMEavg
  return (RSC)
}

.C6calc <- function(
    FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC,
    option = "CFB") {
  # feels like this should make sense, but fails when called from fbp() and not all C6
  # stopifnot("C6" == FUELTYPE)
  RSI <- intermediate_surface_rate_of_spread_c6(ISI)
  # Return at this point, if specified by caller
  if (option == "RSI") {
    return(RSI)
  }
  RSC <- crown_rate_of_spread_c6(ISI, FMC)
  # Return at this point, if specified by caller
  if (option == "RSC") {
    return(RSC)
  }
  RSS <- surface_rate_of_spread_c6(RSI, BUI)
  # Crown Fraction Burned
  CFB <- ifelse(RSC > RSS, .CFBcalc(FUELTYPE, FMC, SFC, RSS, CBH), 0)
  # Return at this point, if specified by caller
  if (option == "CFB") {
    return(CFB)
  }
  # Eq. 65 (FCFDG 1992) Calculate Rate of spread (m/min)
  ROS <- ifelse(RSC > RSS, RSS + (CFB) * (RSC - RSS), RSS)
  return(ROS)
}
