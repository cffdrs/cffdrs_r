#' Back Fire Rate of Spread Calculator
#' 
#' Calculate the Back Fire Spread Rate. All variables names are laid out in the 
#' same manner as Forestry Canada Fire Danger Group (FCFDG) (1992). 
#' 
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068} 
#' Development and Structure of the Canadian Forest Fire Behavior Prediction 
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario. 
#' 
#' @param FUELTYPE    The Fire Behaviour Prediction FuelType
#' @param FFMC        Fine Fuel Moisture Code
#' @param BUI         Buildup Index
#' @param WSV         Wind Speed Vector
#' @param FMC         Foliar Moisture Content
#' @param SFC         Surface Fuel Consumption
#' @param PC          Percent Conifer
#' @param PDF         Percent Dead Balsam Fir
#' @param CC          Degree of Curing (just "C" in FCFDG 1992)
#' @param CBH         Crown Base Height
#' 
#' @return BROS: Back Fire Rate of Spread
#' @noRd

.BROScalc <- function(FUELTYPE, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH){

  #Eq. 46 (FCFDG 1992)
  #Calculate the FFMC function from the ISI equation
  m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
  #Eq. 45 (FCFDG 1992)
  fF <- 91.9 * exp(-0.1386 * m) * (1.0 + (m**5.31) / 4.93e7)
  #Eq. 75 (FCFDG 1992)
  #Calculate the Back fire wind function
  BfW <- exp(-0.05039 * WSV)
  #Calculate the ISI associated with the back fire spread rate
  #Eq. 76 (FCFDG 1992)
  BISI <- 0.208 * BfW * fF
  #Eq. 77 (FCFDG 1992)
  #Calculate final Back fire spread rate
  BROS <- .ROScalc(FUELTYPE, BISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  
  return(BROS)
}