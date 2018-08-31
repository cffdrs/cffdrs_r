#############################################################################
# Description:
#   Simple Simard function to calculate the rate of spread and direction 
#   given one set of three line-based observations of fire arrival time. 
#   The function requires that the user specify time that the fire crossed 
#   each point, along with the measured lengths between each pair of 
#   observational points, and a reference bearing (one specified side of 
#   the triangle). This function allows quick input of a dataframe 
#   specifying one or more many triangles. 
#
#   Units: length in meters, time depends on if user enters minutes or 
#   seconds (regardless, must enter fractional time, e.g, 1.86 minutes)
#
#
# Args:
#   input, a data frame with columns:
#     T1          Time that the fire front crossed points 1.
#     T2          Time that the fire front crossed point 2.
#     T3          Time that the fire front crossed points 3.
#     LENGTHT1T2  Length between each pair of observation points (T1 and T2)
#     LENGTHT2T3  Length between each pair of observation points (T2 and T3)
#     LENGTHT1T3  Length between each pair of observation points (T1 and T3)
#     BEARINGT1T2 Reference bearing in degrees (T1 to T2).
#     BEARINGT1T3 Reference bearing in degrees (T1 to T3).
#   

# Returns:
#   Dataframe containing ROS and Direction
#
#############################################################################
# Simple Simard function for Rate-of-Spread
lros <- function(input) {
  if(is.data.frame(input)){
    # Force uppercase to the column names
    names(input) <- toupper(names(input))
    # Check if the columns exist
    # required column names
    namesTheyShouldBe <- c("T1", "LENGTHT1T2","T2", "LENGTHT1T3", "T3", 
                           "LENGTHT2T3", "BEARINGT1T2", "BEARINGT1T3")
    # Check if all required columns exist
    exist <- !names(input) %in% namesTheyShouldBe
    # If not, stop execution of function and provide error message
    if (any(exist)){
      errmsg <- paste("cffdrs::lros Column ", colnames(input[exist[TRUE]]), 
                      " is required in column list.")

      stop(errmsg)
    }

    
    AngleArad <- acos(((input$LENGTHT1T3^2 + input$LENGTHT1T2^2 - 
                          input$LENGTHT2T3^2) / (2 * input$LENGTHT1T3 * 
                                                   input$LENGTHT1T2)))
    AngleAdeg <- (AngleArad * 180) / (pi)
    ThetaArad <- atan((input$T3-input$T1) / (input$T2 - input$T1) * 
                        (input$LENGTHT1T2 / (input$LENGTHT1T3 * 
                                               sin(AngleArad))) - 
                        (1 / (tan(AngleArad))))
    ThetaAdeg <- (ThetaArad * 180) / (pi)
    DIR <- .direction(input$BEARINGT1T2, input$BEARINGT1T3, ThetaAdeg)
    ROS <- (input$LENGTHT1T2 * cos(ThetaArad)) / (input$T2 - input$T1)
    return(data.frame(Ros = ROS, Direction = DIR))
  } else{
    stop("'input' must be a data.frame") 
  }
}