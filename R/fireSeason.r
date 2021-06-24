#' Fire Season Start and End
#' 
#' \code{\link{fireSeason}} calculates the start and end fire season dates for
#' a given weather station. The current method used in the function is based on
#' three consecutive daily maximum temperature thresholds (Wotton and Flannigan
#' 1993, Lawson and Armitage 2008). This function process input from a single
#' weather station.
#' 
#' An important aspect to consider when calculating Fire Weather Index (FWI)
#' System variables is a definition of the fire season start and end dates
#' (Lawson and Armitage 2008). If a user starts calculations on a fire season
#' too late in the year, the FWI System variables may take too long to reach
#' equilibrium, thus throwing off the resulting indices. This function presents
#' two method of calculating these start and end dates, adapted from Wotton and
#' Flannigan (1993), and Lawson and Armitage (2008). The approach taken in this
#' function starts the fire season after three days of maximum temperature
#' greater than 12 degrees Celsius. The end of the fire season is determined
#' after three consecutive days of maximum temperature less than 5 degrees
#' Celsius.  The two temperature thresholds can be adjusted as parameters in
#' the function call. In regions where temperature thresholds will not end a
#' fire season, it is possible for the fire season to span multiple years, in
#' this case setting the multi.year parameter to TRUE will allow these
#' calculations to proceed.
#' 
#' This fire season length definition can also feed in to the overwinter DC
#' calculations (\link{wDC}). View the cffdrs package help files for an example
#' of using the \code{fireSeason}, \link{wDC}, and \link{fwi} functions in
#' conjunction.
#' 
#' @param input A data.frame containing input variables of including the
#' date/time and daily maximum temperature. Variable names have to be the same
#' as in the following list, but they are case insensitive. The order in which
#' the input variables are entered is not important either.
#' 
#' \tabular{lll}{ 
#' \var{yr} \tab (required) \tab Year of the observations\cr
#' \var{mon} \tab (required) \tab Month of the observations\cr 
#' \var{day} \tab (required) \tab Day of the observations\cr 
#' \var{tmax} \tab (required) \tab Maximum Daily Temperature (degrees C)\cr 
#' \var{snow_depth} \tab (optional) \tab Is consistent snow data in the input?\cr }.
#' 
#' @param fs.start Temperature threshold (degrees C) to start the fire season
#' (default=12)
#' @param fs.end Temperature threshold (degrees C) to end the fire season
#' (default=5)
#' @param method Method of fire season calculation. Options are "wf93"" or
#' "la08" (default=WF93)
#' @param consistent.snow Is consistent snow data in the input? (default=FALSE)
#' @param multi.year Should the fire season span multiple years?
#' (default=FALSE)
#' @return \link{fireSeason} returns a data frame of season and start and end
#' dates. Columns in data frame are described below.
#' 
#' Primary FBP output includes the following 8 variables: 
#' \item{yr }{Year of the fire season start/end date} 
#' \item{mon }{Month of the fire season start/end date} 
#' \item{day }{Day of the fire season start/end date}
#' \item{fsdatetype }{Fire season date type (values are either "start" or "end")} 
#' \item{date}{Full date value}
#' 
#' @author Alan Cantin, Xianli Wang, Mike Wotton, and Mike Flannigan
#' 
#' @seealso \code{\link{fwi}, \link{wDC}}
#' 
#' @references Wotton, B.M. and Flannigan, M.D. (1993). Length of the fire
#' season in a changing climate. Forestry Chronicle, 69, 187-192.
#' 
#' \url{http://www.ualberta.ca/~flanniga/publications/1993_Wotton_Flannigan.pdf}
#' 
#' Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian Forest
#' Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North. For.
#' Cent., Edmonton, AB \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' #The standard test data:
#' data("test_wDC")
#' print(head(test_wDC))
#' ## Sort the data:
#' input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
#' 
#' #Using the default fire season start and end temperature 
#' #thresholds:
#' a_fs <- fireSeason(input[input$id==1,])
#' 
#' #Check the result:
#' a_fs
#' 
#' #    yr mon day fsdatetype
#' #1 1999   5   4      start
#' #2 1999   5  12        end
#' #3 1999   5  18      start
#' #4 1999   5  25        end
#' #5 1999   5  30      start
#' #6 1999  10   6        end
#' #7 2000   6  27      start
#' #8 2000  10   7        end
#' 
#' #In the resulting data frame, the fire season starts 
#' #and ends multiple times in the first year. It is up to the user #for how to interpret this.
#' 
#' #modified fire season start and end temperature thresholds
#' a_fs <- fireSeason (input[input$id==1,],fs.start=10, fs.end=3)
#' a_fs
#' #    yr mon day fsdatetype
#' #1 1999   5   2      start
#' #2 1999  10  20        end
#' #3 2000   6  16      start
#' #4 2000  10   7        end
#' #select another id value, specify method explicitly
#' b_fs <- fireSeason(input[input$id==2,],method="WF93")
#' #print the calculated fireseason
#' b_fs
#' #   yr mon day fsdatetype
#' #1 1980   4  21      start
#' #2 1980   9  19        end
#' #3 1980  10   6      start
#' #4 1980  10  16        end
#' #5 1981   5  21      start
#' #6 1981  10  13        end
#' 
#' @export fireSeason
fireSeason <- function(input, fs.start = 12, fs.end = 5, method = "WF93", 
                       consistent.snow = FALSE, multi.year = FALSE){
  #############################################################################
  # Description:
  #   Calculation of fire season. The intent of this function is to allow
  #   for calculation of the start and end dates of a fire season. Knowledge
  #   of the start and end dates allows for more accurate calculation of Fire
  #   Weather Indices. Only 1 method is presented here, however future 
  #   modifications will allow the inclusion of other methods.
  #
  #   The current method contained within are contained in the following 
  #   article:
  #
  #   Wotton B.M. and Flannigan, M.D. 1993. Length of the fire season in a 
  #     changing climate. Forestry Chronicle, 69: 187-192.
  
  #   **Note that snow depth/cover is not currently included due to a lack of
  #     widespread snow coverage data, however this will be a future addition.
  #
  # Args:
  #   input:    Single station weather data input stream - view fireSeason.Rd 
  #             documentation for full description.
  #   fs.start: Temperature threshold to start the fire season (deg celcius)
  #   fs.end:   Temperature threshold to end the fire season (deg celcius)
  #   method:   Fire Season calculation method. The default and only current
  #             method being used is "WF93".
  #   consistent.snow  TRUE/FALSE value if consistent snow data is available
  #             if it is not, then we will default to WF93.
  #   
  #   
  # Returns:
  #   seasonStartEnd: data.frame containing start and end dates of fire
  #                   seasons for a particular weather station.
  #
  #############################################################################
  names(input) <- tolower(names(input))
  #Ensure that required variables exist, if not, stop and give message
  # If variables exist, conver to local scope variables
  if ("yr" %in% names(input)) {
    yr <- input$yr
  } else {
    stop("Year was not provided, year is required for this function.")
  }
  if ("mon" %in% names(input)) {
    mon <- input$mon
    if(!all(mon %in% seq(1, 12))){
      stop("mon value is out of bounds (1-12), stopping execution.")
    }
  } else {
    stop("Month was not provided, month is required for this function.")
  }
  if ("day" %in% names(input)) {
    day <- input$day
    if(!all(day %in% seq(1, 31))){
      stop("day value is out of bounds (1-31), stopping execution.")      
    }
  } else {
    stop("day was not provided, day is required for this function, stopping 
         execution..")
  }
  
  if ("tmax" %in% names(input)) {
    tmax <- input$tmax
  } else {
    stop("Maximum Daily Temperature (tmax) was not provided, daily tmax is 
         required for this function.")
  }
  method <- tolower(method)
  if(method != "wf93" & method != "la08"){
    stop(paste("Selected method \"", method, "\" is unavailable, read 
               documentation for available methods.",sep=""))
  }
  
  #Get length of dataset
  n0<-length(tmax)
  seasonActive <- FALSE
  seasonStartEnd <- NULL
  
  #Run Wotton & Flannign 1993 fire season calculation method (without snow)
  if(method == "wf93"){
    for (k in 1:n0){
      if (k > 3){
        #If we are not already in a fire season AND
        #  the last 3 days are all greater than the starting temperature 
        #  threshold, then set start date.
        if(!seasonActive & (all(tmax[seq(k-3, k-1, 1)] > fs.start))){
          seasonActive <- TRUE #set season to active
          theday <- day[k]
          #If the data is starting us on January 4th, then we should have 
          #  started on January 1st Should be modified to work on any starting 
          #  data, but for now, just calendar year to allow for year-round 
          #  calculations.
          if (!multi.year){
            if(mon[k] == 1 && day[k] == 4){
              theday <- day[k-3]
            }
          }
          #Bind the start date to the data.frame
          seasonStartEnd <- rbind(seasonStartEnd, 
                                  data.frame(yr = yr[k], 
                                             mon = mon[k],
                                             day = theday,
                                             fsdatetype = "start"))
        }
        #I we are already in a fire season AND
        #  the last 3 days are less than the end season temperature threshold
        #  thn set end date.
        if(seasonActive & all(tmax[seq(k-3, k-1, 1)] < fs.end)){
          seasonActive <- FALSE
          #Bind the end date to the data frame.
          seasonStartEnd <- rbind(seasonStartEnd, 
                                  data.frame(yr = yr[k],
                                             mon = mon[k],
                                             day = day[k],
                                             fsdatetype = "end"))
        }
      }
    }
  #Run the CFS1984 / Lawson and Armitage 2008 combined procedure
  } else {
    if(method == "la08"){
      # In regions normally covered by snow, begin calculations 3rd day after snow has left the area
      if(consistent.snow){
        # Check that data has snow_depth variable
        if(!("snow_depth" %in% names(input))){
          stop(paste("Snow depth is required for the selected method \"", method, "\", read
                     documentation for appropriate use.",sep=""))
        } else {
          # set a local snow_depth variable
          snow_depth = input$snow_depth
          for (k in 1:n0){
            # Must be more than 3 days in the dataset to test this out
            if (k > 3){
              #If we are not already in a fire season AND
              #  the last 3 days (includsive) are all snow free then set the starting date
              if(!seasonActive & (all(snow_depth[seq(k-2, k, 1)] <= 0))){
                seasonActive <- TRUE #set season to active
                theday <- day[k]
                # If running annual seasons only, and if the data is starting us on January 4th, then we should have 
                #  started on January 1st Should be modified to work on any starting 
                #  data, but for now, just calendar year to allow for year-round 
                #  calculations.
                if (!multi.year){
                  if(mon[k] == 1 && day[k] == 4){
                    theday <- day[k-3]
                  }
                }
                #Bind the start date to the data.frame
                seasonStartEnd <- rbind(seasonStartEnd, 
                                        data.frame(yr = yr[k], 
                                                   mon = mon[k],
                                                   day = theday,
                                                   fsdatetype = "start"))
              }
              #  Stop the season if we are already in a fire season AND
              #  there is a day with snow on the ground OR it is after 
              #  November 30, and temperatures dip below the fs.end 
              #  threshold for LA08, which is 12C by default.
              if(seasonActive & 
                 (snow_depth[k] > 0 |
                   (mon[k] == 12 &
                   all(tmax[seq(k-2, k, 1)] < fs.end)))
                  ){
                seasonActive <- FALSE
                #Bind the end date to the data frame.
                seasonStartEnd <- rbind(seasonStartEnd, 
                                        data.frame(yr = yr[k],
                                                   mon = mon[k],
                                                   day = day[k],
                                                   fsdatetype = "end"))
              }
            }
          }
        }
      }
      # Not an area of consistent snow, so run the wf93 algorithm
      else{
        fireSeason(input, fs.start, fs.end, method = "WF93")
      }
    }
  }
  # If there is a start and an end on the same date, remove both
  # This may happen when starting/ending in Dec for LA08 method
  seasonStartEnd$date <- as.Date(as.POSIXlt(paste(seasonStartEnd$yr, "-", seasonStartEnd$mon, "-", seasonStartEnd$day,sep="")))
  dups <- seasonStartEnd[duplicated(seasonStartEnd[,0:3]),]
  seasonStartEnd <- seasonStartEnd[!seasonStartEnd$date %in% dups$date, ]
  
  #Return the season start end dates data.frame to caller
  return(seasonStartEnd)
}
