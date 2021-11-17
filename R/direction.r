#' Direction definer
#' 
#' New DIRECTION function to determine clockwise or counter-clockwise 
#' "interpretation" 
#' 
#' @param bearingT1T2 Bearing between T1 and T2
#' @param bearingT1T3 Bearing between T1 and T3
#' @param ThetaAdeg Direction
#' 
#' @return DIR - a direction in degrees
#' 
#' @noRd

.direction <- function(bearingT1T2, bearingT1T3, ThetaAdeg){

  T1T2 <- bearingT1T2
  T1T3 <- bearingT1T3
  DIR <- rep(NA, length(T1T2))
  DIR <- ifelse(T1T2 > 0 & T1T3 > 0 & T1T2 > T1T3, T1T2 - ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 > 0 & T1T3 > 0 & T1T2 < T1T3, T1T2 + ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 < 0 & T1T3 < 0 & T1T2 > T1T3, T1T2 - ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 < 0 & T1T3 < 0 & T1T2 < T1T3, T1T2 + ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 > 0 & T1T2 < 90 & T1T3 < 0 & T1T3 > -90, 
                T1T2 - ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 < 0 & T1T2 > -90 & T1T3 > 0 & T1T3 < 90, 
                T1T2 + ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 > 90 & T1T3 < -90 & T1T2 + ThetaAdeg > 180, 
                T1T2 + ThetaAdeg - 360, DIR)
  DIR <- ifelse(T1T2 > 90 & T1T3 < -90 & T1T2 + ThetaAdeg < 180, 
                T1T2 + ThetaAdeg, DIR)
  DIR <- ifelse(T1T2 < -90 & T1T3 > 90 & T1T2 - ThetaAdeg < -180, 
                T1T2 - ThetaAdeg + 360, DIR)
  DIR <- ifelse(T1T2 < -90 & T1T3 > 90 & T1T2 - ThetaAdeg > -180, 
                T1T2 - ThetaAdeg, DIR)
  DIR <- ifelse(DIR < -180, DIR + 10, DIR)
  DIR <- ifelse(DIR > 180, DIR - 10, DIR)
  return(DIR)
}
