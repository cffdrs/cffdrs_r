.direction <- function(bearingT1T2, bearingT1T3, ThetaAdeg){
  #############################################################################
  # Description:
  #   New DIRECTION function to determine clockwise or counter-clockwise 
  #   "interpretation" 
  #
  #
  # Args:
  #   beaaringT1T2  
  # Returns:
  #   DIR   a direction in degrees
  #
  #############################################################################

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
