pros <- function(input) {
  #############################################################################
  # Description:
  #   Simple Simard function to calculate the rate of spread and direction 
  #   given one set of three point-based observations of fire arrival time. 
  #   The function requires that the user specify time that the fire crossed 
  #   each point, along with the latitude and longitude of each observational 
  #   point. This function allows quick input for a single triangle or a 
  #   dataframe specifying many triangles. 
  #
  #   Units: length in meters, time depends on if user enters minutes or 
  #   seconds (regardless, must enter fractional time, e.g, 1.86 minutes)
  #
  #
  # Args:
  #   input, a data frame with columns:
  #     T1    Time fire front crossed point 1
  #     LONG1 Longitude of datalogger 1
  #     LAT1  Latitude of datalogger 1
  #     T2    Time fire front crossed point 2
  #     LONG2 Longitude of datalogger 2
  #     LAT2  Latitude of datalogger 2
  #     T3    Time fire front crossed point 3
  #     LONG3 Longitude of datalogger 3
  #     LAT3  Latitude of datalogger 3
  # Returns:
  #   Dataframe containing ROS and Direction
  #
  #############################################################################
  
  # Force uppercase to the column names
  names(input) <- toupper(names(input))
  # Check if the columns exist
  # required column names
  namesTheyShouldBe <- c("T1", "LONG1", "LAT1", "T2", "LONG2", "LAT2", "T3", 
                         "LONG3", "LAT3")
  # Check if all required columns exist
  exist <- !names(input) %in% namesTheyShouldBe
  # If not, stop execution of function and provide error message
  if (any(exist)){
    errmsg <- paste("cffdrs::pros Column ", colnames(input[exist[TRUE]]), 
                    " is required in column list.")
    
    stop(errmsg)
  }
  LengthT1T2 <- geosphere::distHaversine(cbind(input$LONG1, input$LAT1), 
                                         cbind(input$LONG2, input$LAT2))
  LengthT1T3 <- geosphere::distHaversine(cbind(input$LONG1, input$LAT1), 
                                         cbind(input$LONG3, input$LAT3))
  LengthT2T3 <- geosphere::distHaversine(cbind(input$LONG2, input$LAT2), 
                                         cbind(input$LONG3, input$LAT3))
  bearingT1T2 <- geosphere::bearing(cbind(input$LONG1, input$LAT1),
                         cbind(input$LONG2, input$LAT2))
  bearingT1T3 <- geosphere::bearing(cbind(input$LONG1, input$LAT1),
                         cbind(input$LONG3, input$LAT3))
  bearingT2T3 <- geosphere::bearing(cbind(input$LONG2, input$LAT2),
                         cbind(input$LONG3, input$LAT3))
  RefT1T2bearing <- geosphere::bearing(cbind(input$LONG1, input$LAT1),
                            cbind(input$LONG2, input$LAT2))
  AngleArad <- acos(((LengthT1T3^2 + LengthT1T2^2 - LengthT2T3^2) / 
                       (2 * LengthT1T3 * LengthT1T2)))
  AngleAdeg <- (AngleArad*180) / (pi)
  ThetaArad <- atan((input$T3 - input$T1) / (input$T2-input$T1) * 
                      (LengthT1T2 / (LengthT1T3 * sin(AngleArad)))
                    - (1 / (tan(AngleArad))))
  ThetaAdeg <- (ThetaArad * 180) / (pi)
  DIR <- .direction(bearingT1T2, bearingT1T3, ThetaAdeg)
  ROS <- (LengthT1T2 * cos(ThetaArad)) / (input$T2-input$T1)
  
  return(data.frame(Ros = ROS, Direction = DIR))
}