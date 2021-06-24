#' Overwintering Drought Code
#' 
#' \code{wDC} calculates an initial or season starting Drought Code (DC) value
#' based on a standard method of overwintering the Drought Code (Lawson and
#' Armitage 2008).  This method uses the final DC value from previous year,
#' over winter precipitation and estimates of how much over-winter
#' precipitation 'refills' the moisture in this fuel layer. This function could
#' be used for either one weather station or for multiple weather stations.
#' 
#' Of the three fuel moisture codes (i.e. FFMC, DMC and DC) making up the FWI
#' System, only the DC needs to be considered in terms of its values carrying
#' over from one fire season to the next.  In Canada both the FFMC and the DMC
#' are assumed to reach moisture saturation from overwinter precipitation at or
#' before spring melt; this is a reasonable assumption and any error in these
#' assumed starting conditions quickly disappears.  If snowfall (or other
#' overwinter precipitation) is not large enough however, the fuel layer
#' tracked by the Drought Code may not fully reach saturation after spring snow
#' melt; because of the long response time in this fuel layer (53 days in
#' standard conditions) a large error in this spring starting condition can
#' affect the DC for a significant portion of the fire season.  In areas where
#' overwinter precipitation is 200 mm or more, full moisture recharge occurs
#' and DC overwintering is usually unnecessary.  More discussion of
#' overwintering and fuel drying time lag can be found in Lawson and Armitage
#' (2008) and Van Wagner (1985).
#' 
#' @param DCf Final fall DC value from previous year
#' @param rw Winter precipitation (mm)
#' @param a User selected values accounting for carry-over fraction (view table
#' below)
#' @param b User selected values accountain for wetting efficiency fraction
#' (view table below)
#' @return \code{wDC} returns either a single value or a vector of wDC values.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, and Mike Flannigan
#' @seealso \code{\link{fwi}}, \code{\link{fireSeason}}
#' @references Lawson B.D. and Armitage O.B. 2008. Weather Guide for the
#' Canadian Forest Fire Danger Rating System. Natural Resources Canada,
#' Canadian Forest Service, Northern Forestry Centre, Edmonton, Alberta. 84 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#' 
#' Van Wagner, C.E. 1985. Drought, timelag and fire danger rating. Pages
#' 178-185 in L.R. Donoghue and R.E. Martin, eds. Proc. 8th Conf. Fire For.
#' Meteorol., 29 Apr.-3 May 1985, Detroit, MI. Soc. Am. For., Bethesda, MD.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/23550.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' # The standard test data:
#' data("test_wDC")
#' # (1) Simple case previous fall's DC was 300, overwinter 
#' # rain 110mm
#' winter_DC <- wDC(DCf=300,rw=110)
#' winter_DC
#' #(2) modified a and b parameters. Find table values in listed 
#' # reference for Lawson and Armitage, 2008.
#' winter_DC <- wDC(DCf=300,rw=110,a=1.0,b=0.9)
#' winter_DC
#' #(3)with multiple inputs:
#' winter_DC <- wDC(DCf=c(400,300,250), rw=c(99,110,200),
#'                    a=c(0.75,1.0,0.75), b=c(0.75,0.9,0.75))
#' winter_DC
#' #(4) A realistic example:
#' #precipitation accumulation and date boundaries
#' input <- test_wDC
#' #order data by ID and date
#' input <- with(input,input[order(id,yr,mon,day),])
#' input$date <- as.Date(as.POSIXlt(paste(input$yr,"-",input$mon,"-",input$day,sep="")))
#' #select id value 1
#' input.2 <- input[input$id==2,]
#' #Explicitly defined fire start and end dates.
#' data("test_wDC_fs")
#' print(test_wDC_fs)
#' #Set date field
#' test_wDC_fs$date <- as.Date(as.POSIXlt(paste(test_wDC_fs$yr,"-",test_wDC_fs$mon,"-",
#'                                              test_wDC_fs$day,sep="")))
#' #match to current id value
#' input.2.fs <- test_wDC_fs[test_wDC_fs$id==2,]
#' #assign start of winter date (or end of fire season date)
#' winterStartDate <- input.2.fs[2,"date"]
#' #assign end of winter date (or start of new fire season date)
#' winterEndDate <-  input.2.fs[3,"date"]
#' #Accumulate overwinter precip based on chosen dates
#' curYr.prec <- sum(input.2[(input.2$date>winterStartDate & input.2$date < winterEndDate),]$prec)
#' #Assign a fall DC value
#' fallDC <- 500
#' #calculate winter DC
#' winter_DC <- wDC(DCf=fallDC,rw=curYr.prec)
#' winter_DC
#' #Assign a different fall DC value
#' fallDC <- 250
#' #calculate winter DC
#' winter_DC <- wDC(DCf=fallDC,rw=curYr.prec,a=1.0)
#' winter_DC
#' 
#' @export wDC
wDC <- function(DCf = 100, rw = 200, a = 0.75, b = 0.75) {
  #############################################################################
  # Description:
  #   Computes the over wintering Drought Code (DC) value.
  #   All variables names are laid out in the same manner as Lawson & Armitage
  #   (2008).
  
  #   Lawson B.D. and Armitage O.B. 2008. Weather Guide for the Canadian Forest 
  #   Fire Danger Rating System. Natural Resources Canada, Canadian Forest 
  #   Service, Northern Forestry Centre, Edmonton, Alberta. 84 p. 
  #   http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf
  #
  # Args:
  #     DCf: Final Fall DC Value
  #      rw: winter precipitation (mm)
  #       a: user-selected value accounting for carry-over fraction
  #       b: user-selected value accounting for wetting efficiency fraction
  # Returns:
  #     DCs: Overwintered Drought Code (Spring startup DC value)
  #
  #############################################################################
  #Eq. 3 - Final fall moisture equivalent of the DC
  Qf <- 800 * exp(-DCf / 400)
  #Eq. 2 - Starting spring moisture equivalent of the DC
  Qs <- a * Qf + b * (3.94 * rw)
  #Eq. 4 - Spring start-up value for the DC
  DCs <- 400 * log(800 / Qs)
  #Constrain DC
  DCs <- ifelse(DCs < 15, 15, DCs)
  return(DCs)
}

