#' Crown Fraction Burned Calculator
#' 
#' Calculate Calculate Crown Fraction Burned. To calculate CFB, we also need to 
#' calculate Critical surface intensity (CSI), and Surface fire rate of spread 
#' (RSO). The value of each of these equations can be returned to the calling 
#' function without unecessary additional calculations.
#' 
#' All variables names are laid out in the same manner as Forestry Canada Fire 
#' Danger Group (FCFDG) (1992). Development and Structure of the Canadian Forest
#'  Fire Behavior Prediction System." Technical Report ST-X-3, Forestry Canada,
#'   Ottawa, Ontario.
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
#' @param option   Which variable to calculate(ROS, CFB, RSC, or RSI) 
#' 
#' @return CFB, CSI, RSO depending on which option was selected. _Default:_ "CFB"
#' @noRd

.CFBcalc <- function(FUELTYPE, FMC, SFC, ROS, CBH, option="CFB"){
  
  CFB <- 0
  #Eq. 56 (FCFDG 1992) Critical surface intensity
  CSI <- 0.001 * (CBH**1.5) * (460 + 25.9 * FMC)**1.5
  #Return at this point, if specified by caller
  if(option=="CSI"){
    return(CSI)
  }
  #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
  RSO <- CSI / (300 * SFC)
  #Return at this point, if specified by caller
  if(option=="RSO"){
    return(RSO)
  }
  #Eq. 58 (FCFDG 1992) Crown fraction burned 
  CFB <- ifelse(ROS > RSO, 1 - exp(-0.23 * (ROS - RSO)), CFB)
  return(CFB)
}
