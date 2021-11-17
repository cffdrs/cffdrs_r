#' Line-based input for Simard Rate of Spread and Direction
#' 
#' \code{lros} is used to calculate the rate of spread and direction given one
#' set of three point-based observations of fire arrival time. The function
#' requires that the user specify the time that the fire crossed each point,
#' along with the measured lengths between each pair of observational points,
#' and a reference bearing (one specified side of the triangle). This function
#' allows quick input of a dataframe specifying one or many triangles.
#' 
#' \code{lros} Allows R users to calculate the rate of spread and direction of
#' a fire across a triangle, given three time measurements and details about
#' the orientation and distance between observational points. The algorithm is
#' based on the description from Simard et al. (1984). See \code{pros} for more
#' information.
#' 
#' The functions require the user to arrange the input dataframe so that each
#' triangle of interest is identified based on a new row in the dataframe. The
#' input format forces the user to identify the triangles, one triangle per row
#' of input dataframe. Very complex arrangements of field plot layouts are
#' possible, and the current version of these functions do not attempt to
#' determine each triangle of interest automatically.
#' 
#' @param input A dataframe containing input variables of time fire front
#' crossed points 1, 2, 3, and latitude/longitude for those same points.
#' Variable names have to be the same as in the following list, but they are
#' case insensitive. The order in which the input variables are entered is not
#' important.
#' 
#' \tabular{lll}{ 
#' \var{T1} 
#' \tab (required) 
#' \tab Time that the fire front crossed point 1.\cr
#' \tab\tab Time entered in fractional format. \cr
#' \tab\tab Output ROS will depend on the level \cr
#' \tab\tab of precision entered \cr
#' \tab\tab (minute, second, decisecond)\cr 
#' \var{T2} 
#' \tab (required) 
#' \tab Time that the fire front crossed point 2.\cr
#' \tab\tab Time entered in fractional format. \cr
#' \tab\tab Output ROS will depend on the level \cr
#' \tab\tab of precision entered \cr
#' \tab\tab (minute, second, decisecond)\cr 
#' \var{T3} 
#' \tab (required) 
#' \tab Time that the fire front crossed point 3. \cr 
#' \tab\tab Time entered in fractional format. \cr
#' \tab\tab Output ROS will depend on the level \cr
#' \tab\tab of precision entered \cr
#' \tab\tab (minute, second, decisecond)\cr 
#' \var{LengthT1T2}
#' \tab (required) \tab Length between each pair of\cr
#' \tab\tab observation points T1 and T2 (subscripts \cr
#' \tab\tab denote time-ordered pairs). (meters)\cr 
#' \var{LengthT2T3}
#' \tab (required) 
#' \tab Length between each pair of\cr
#' \tab\tab observation points T2 and T3 (subscripts \cr
#' \tab\tab denote time-ordered pairs). (meters)\cr 
#' \var{LengthT1T3}
#' \tab (required) 
#' \tab Length between each pair of\cr
#' \tab\tab observation points T1 and T3 (subscripts \cr
#' \tab\tab denote time-ordered pairs). (meters)\cr 
#' \var{BearingT1T2} 
#' \tab (required) 
#' \tab Reference bearing. For reference,\cr
#' \tab\tab North = 0, West = -90, East = 90 (degrees)\cr 
#' \var{BearingT1T3} 
#' \tab (required) 
#' \tab Reference bearing. For reference,\cr 
#' \tab\tab North = 0, West = -90, East = 90 (degrees)\cr 
#' }
#' @return \code{lros} returns a dataframe which includes the rate of spread
#' and spread direction. Output units depend on the user’s inputs for
#' distance (typically meters) and time (seconds or minutes).
#' @author Tom Schiks, Xianli Wang, Alan Cantin
#' @seealso \code{\link{pros}},
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
#' # manual single entry, but converted to a data frame
#' lros.in1 <- data.frame(t(c(0, 24.5, 50, 22.6, 120, 20.0, 90, 35)))
#' colnames(lros.in1)<-c("T1","LengthT1T2", "T2", "LengthT1T3", "T3", 
#'                       "LengthT2T3", "bearingT1T2", "bearingT1T3")
#' lros.out1 <- lros(lros.in1)
#' lros.out1
#' 
#' # multiple entries using a dataframe
#' # load the test dataframe for lros
#' data("test_lros")
#' lros(test_lros)
#' 
#' 
#' 
#' @export lros
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
