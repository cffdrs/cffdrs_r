#' Flank Fire Rate of Spread Calculator
#' 
#' Calculate the Flank Fire Spread Rate. 
#' 
#' All variables names are laid out in the same manner as Forestry Canada 
#' Fire Danger Group (FCFDG) (1992). Development and Structure of the 
#' Canadian Forest Fire Behavior Prediction System." Technical Report 
#' ST-X-3, Forestry Canada, Ottawa, Ontario.
#' 
#' @param ROS  Fire Rate of Spread (m/min)
#' @param BROS Back Fire Rate of Spread (m/min) 
#' @param LB   Length to breadth ratio
#' 
#' @return FROS Flank Fire Spread Rate (m/min) value
#' 
#' @noRd

.FROScalc <- function(ROS, BROS, LB){

  #Eq. 89 (FCFDG 1992)
  FROS <- (ROS + BROS) / LB / 2
  return(FROS)
}
