#' Point-based input for Simard Rate of Spread and Direction
#' 
#' \code{pros} is used to calculate the rate of spread and direction given one
#' set of three point-based observations of fire arrival time. The function
#' requires that the user specify the time that the fire crossed each point,
#' along with the latitude and longitude of each observational point. This
#' function allows quick input of a dataframe specifying one or many triangles.
#' 
#' \code{pros} allows R users to calculate the rate of spread and direction of
#' a fire across a triangle, given three time measurements and details about
#' the orientation and distance between observational points. The algorithm is
#' based on the description from Simard et al. (1984).
#' 
#' Rate of spread and direction of spread are primary variables of interest
#' when observing wildfire growth over time. Observations might be recorded
#' during normal fire management operations (e.g., by a Fire Behaviour
#' Analyst), during prescribed fire treatments, and during experimental
#' research burns. Rate of spread is especially important for estimating
#' Byram's fireline intensity, fireline intensity = heat constant of fuel ×
#' weight of fuel consumed × forward rate of spread (Byram 1959).
#' 
#' Rate of spread is difficult to measure and highly variable in the field.
#' Many techniques were proposed over the years, but most were based on
#' observations collected from a pre-placed reference grid and stopwatch (Curry
#' and Fons 1938; Simard et al. 1982). Early approaches required that observers
#' be in visual contact with the reference grid, but later, thermocouples and
#' dataloggers were employed to measure the onset of the heat pulse at each
#' point.
#' 
#' Simard et al. (1982) proposed calculations for spread based on an
#' equilateral triangle layout. Simard et al. (1984) proposed calculations for
#' spread based on any type of triangle. Both articles also discussed field
#' sampling design and layout, with special attention to the size of the
#' triangles (large enough that the fire traverses the triangle in one to two
#' minutes) and even using triangles of varying size within one field plot (but
#' no triangle larger than one fourth of the site's total area).
#' 
#' The underlying algorithms use trigonometry to solve for rate of spread and
#' direction of spread. One important assumption is that the spread rate and
#' direction is uniform across one triangular plot, and that the fire front is
#' spreading as a straight line; Simard et al. (1982, 1984) acknowledge that
#' these assumption are likely broken to some degree during fire spread events.
#' 
#' The functions require the user to arrange the input dataframe so that each
#' triangle of interest is identified based on a new row in the dataframe. The
#' input format forces the user to identify the triangles, one triangle per row
#' of input dataframe. Very complex arrangements of field plot layouts are
#' possible, and the current version of these functions do not attempt to
#' determine each triangle of interest automatically.
#' 
#' @param input A dataframe containing input variables of Time fire front
#' crossed points 1, 2, 3, and latitude/longitude for those same points.
#' Variable names have to be the same as in the following list, but they are
#' case insensitive. The order in which the input variables are entered is not
#' important.
#' 
#' \tabular{lll}{ 
#' \var{T1} \tab (required) \tab Time that the fire front
#' crossed point 1. Time entered in fractional \cr\tab\tab format. Output ROS
#' will depend on the level of precision entered \cr\tab\tab (minute, second,
#' decisecond)\cr 
#' \var{T2} \tab (required) \tab Time that the fire front
#' crossed point 2. Time entered in fractional \cr\tab\tab format. Output ROS
#' will depend on the level of precision entered \cr\tab\tab (minute, second,
#' decisecond)\cr 
#' \var{T3} \tab (required) \tab Time that the fire front crossed point 3. Time 
#' entered in fractional \cr\tab\tab format. Output ROS will depend on the level
#'  of precision entered \cr\tab\tab (minute, second,
#' decisecond)\cr 
#' \var{Long1}\tab (required) \tab Longitude for datalogger 1. (decimal degrees). \cr 
#' \var{Long2}\tab (required) \tab Longitude for datalogger 2. (decimal degrees). \cr 
#' \var{Long3}\tab (required) \tab Longitude for datalogger 3. (decimal degrees). \cr 
#' \var{Lat1} \tab (required) \tab Latitude for datalogger 1. (decimal degrees). \cr 
#' \var{Lat2} \tab (required) \tab Latitude for datalogger 2. (decimal degrees). \cr
#' \var{Lat3} \tab (required) \tab Latitude for datalogger 3. (decimal
#' degrees). \cr }
#' @return \code{pros} returns a dataframe which includes the rate of spread
#' and spread direction. Output units depend on the user’s inputs for
#' distance (typically meters) and time (seconds or minutes).
#' @author Tom Schiks, Xianli Wang, Alan Cantin
#' @seealso \code{\link{lros}},
#' @references 1. Simard, A.J., Eenigenburg, J.E., Adams, K.B., Nissen, R.L.,
#' Deacon, and Deacon, A.G. 1984. A general procedure for sampling and
#' analyzing wildland fire spread.
#' 
#' 2. Byram, G.M. 1959. Combustion of forest fuels. In: Davis, K.P. Forest Fire
#' Control and Use. McGraw-Hill, New York.
#' 
#' 3. Curry, J.R., and Fons, W.L. 1938. Rate of spread of surface fires in the
#' Ponderosa Pine Type of California. Journal of Agricultural Research 57(4):
#' 239-267.
#' 
#' 4. Simard, A.J., Deacon, A.G., and Adams, K.B. 1982. Nondirectional sampling
#' wildland fire spread. Fire Technology: 221-228.
#' @keywords ros
#' @examples
#' 
#' library(cffdrs)
#' # manual single entry
#' pros.in1 <- data.frame(t(c(2, -79.701027, 43.808872, 50, -79.699650, 43.808833
#'                             , 120, -79.700387, 43.809816)))
#' colnames(pros.in1)<-c("T1", "LONG1", "LAT1", "T2", "LONG2", "LAT2", "T3", 
#'                       "LONG3", "LAT3")
#' pros.out1 <- pros(pros.in1)
#' # multiple entries using a dataframe
#' # load the test dataframe for pros
#' data("test_pros")
#' pros(test_pros)
#' 
#' @export pros
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
