#' Fire Weather Index System
#' 
#' \code{fwi} is used to calculate the outputs of the Canadian Forest Fire
#' Weather Index (FWI) System for one day or one fire season based on noon
#' local standard time (LST) weather observations of temperature, relative
#' humidity, wind speed, and 24-hour rainfall, as well as the previous day's
#' fuel moisture conditions. This function could be used for either one weather
#' station or for multiple weather stations.
#' 
#' The Canadian Forest Fire Weather Index (FWI) System is a major subsystem of
#' the Canadian Forest Fire Danger Rating System, which also includes Canadian
#' Forest Fire Behavior Prediction (FBP) System. The modern FWI System was
#' first issued in 1970 and is the result of work by numerous researchers from
#' across Canada. It evolved from field research which began in the 1930's and
#' regional fire hazard and fire danger tables developed from that early
#' research.
#' 
#' The modern System (Van Wagner 1987) provides six output indices which
#' represent fuel moisture and potential fire behavior in a standard pine
#' forest fuel type. Inputs are a daily noon observation of fire weather, which
#' consists of screen-level air temperature and relative humidity, 10 meter
#' open wind speed and 24 accumulated precipitation.
#' 
#' The first three outputs of the system (the Fire Fuel Moisture Code (ffmc),
#' the Duff Moisture Code (dmc), and the Drought Code (dc)) track moisture in
#' different layers of the fuel making up the forest floor. Their calculation
#' relies on the daily fire weather observation and also, importantly, the
#' moisture code value from the previous day as they are in essence bookkeeping
#' systems tracking the amount of moisture (water) in to and out of the layer.
#' It is therefore important that when calculating FWI System outputs over an
#' entire fire season, an uninterrupted daily weather stream is provided; one
#' day is the assumed time step in the models and thus missing data must be
#' filled in.
#' 
#' The next three outputs of the System are relative (unitless) indicators of
#' aspects of fire behavior potential: spread rate (the Initial Spread Index,
#' isi), fuel consumption (the Build-up Index, bui) and fire intensity per unit
#' length of fire front (the Fire Weather Index, fwi).  This final index, the
#' fwi, is the component of the System used to establish the daily fire danger
#' level for a region and communicated to the public.  This final index can be
#' transformed to the Daily Severity Rating (dsr) to provide a more
#' reasonably-scaled estimate of fire control difficulty.
#' 
#' Both the Duff Moisture Code (dmc) and Drought Code (dc) are influenced by
#' day length (see Van Wagner 1987). Day length adjustments for different
#' ranges in latitude can be used (as described in Lawson and Armitage 2008
#' (\url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf})) and are included
#' in this R function; latitude must be positive in the northern hemisphere and
#' negative in the southern hemisphere.
#' 
#' The default initial (i.e., "start-up") fuel moisture code values (FFMC=85,
#' DMC=6, DC=15) provide a reasonable set of conditions for most springtime
#' conditions in Canada, the Northern U.S., and Alaska. They are not suitable
#' for particularly dry winters and are presumably not appropriate for
#' different parts of the world.
#' 
#' @param input A dataframe containing input variables of daily weather
#' observations taken at noon LST. Variable names have to be the same as in the
#' following list, but they are case insensitive. The order in which the input
#' variables are entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{id} \tab (optional) 
#' \tab Unique identifier of a weather\cr
#' \tab\tab station or spatial point (no restriction on\cr
#' \tab\tab data type); required when \code{batch=TRUE}\cr 
#' \var{lat} \tab (recommended) \tab Latitude (decimal degree, default=55)\cr 
#' \var{long} \tab (optional) \tab Longitude (decimal degree)\cr 
#' \var{yr} \tab (optional) \tab Year of observation;
#' required when \code{batch=TRUE}\cr 
#' \var{mon} \tab (recommended) \tab Month of the year (integer 1-12, default=7)\cr 
#' \var{day} \tab (optional) \tab Dayof the month (integer); required when \code{batch=TRUE}\cr 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr 
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 24-hour rainfall (mm)\cr }
#' 
#' @param init A data.frame or vector contains either the initial values for
#' FFMC, DMC, and DC or the same variables that were calculated for the
#' previous day and will be used for the current day's calculation. The
#' function also accepts a vector if the initial or previous day FWI values is
#' for only one weather station (a warning message comes up if a single set of
#' initial values is used for multiple weather stations). Defaults are the
#' standard initial values for FFMC, DMC, and DC defined as the following:
#' \tabular{lll}{ 
#' \bold{Variable} \tab \bold{Description} \tab \bold{Default} \cr
#' \var{ffmc} \tab Previous day Fine Fuel Moisture Code (FFMC; unitless) \tab 85 \cr
#' \var{dmc} \tab Previous day Duff Moisture Code (DMC; unitless)\tab 6 \cr
#' \var{dc} \tab Previous Day Drought Code (DC; unitless) \tab 15\cr
#' \var{lat} \tab Latitude of the weather station (\emph{Optional}) \tab 55 \cr}
#' 
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate daily FWI System
#' outputs for one weather station over a period of time chronologically with
#' the initial conditions given (\code{init}) applied only to the first day of
#' calculation. If multiple weather stations are processed, an additional "id"
#' column is required in the input to label different stations, and the data
#' needs to be sorted by date/time and "id".  If \code{batch=FALSE}, the
#' function calculates only one time step (1 day) base on either the initial
#' start values or the previous day's FWI System variables, which should also
#' be assigned to \code{init} argument.
#' 
#' @param out The function offers two output options, \code{out="all"} will
#' produce a data frame that includes both the input and the FWI System
#' outputs; \code{out="fwi"} will generate a data frame with only the FWI
#' system components.
#' 
#' @param lat.adjust The function offers options for whether day length
#' adjustments should be applied to the calculations.  The default value is
#' "TRUE".
#' 
#' @param uppercase Output in upper cases or lower cases would be decided by
#' this argument. Default is TRUE.
#' 
#' @return \code{fwi} returns a dataframe which includes both the input and the
#' FWI System variables as described below: 
#' \item{Input Variables }{Including temp, rh, ws, and prec with id, long, lat, yr, mon, or day as optional.}
#' \item{ffmc }{Fine Fuel Moisture Code} 
#' \item{dmc }{Duff Moisture Code}
#' \item{dc }{Drought Code} 
#' \item{isi }{Initial Spread Index} 
#' \item{bui }{Buildup Index} 
#' \item{fwi }{Fire Weather Index} 
#' \item{dsr }{Daily Severity Rating}
#' 
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' 
#' @seealso \code{\link{fbp}}, \code{\link{fwiRaster}}, \code{\link{gfmc}},
#' \code{\link{hffmc}}, \code{\link{hffmcRaster}}, \code{\link{sdmc}},
#' \code{\link{wDC}}, \code{\link{fireSeason}}
#' 
#' @references 1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and
#' FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For.
#' Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf}
#' 
#' 2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest
#' fire weather index system. Forest Technology Report 35. (Canadian Forestry
#' Service: Ottawa). \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf}
#' 
#' 3.  Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian
#' Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North.
#' For. Cent., Edmonton, AB.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#' 
#' @keywords methods
#' 
#' @examples
#' 
#' # library(cffdrs)
#' # The test data is a standard test
#' # dataset for FWI system (Van Wagner and Pickett 1985) 
#' # data("test_fwi")
#' # Show the data, which is already sorted by time:
#' # head(test_fwi)
#' # long  lat	yr	mon	day	temp	rh	ws	prec
#' # -100	40	1985	4	  13	17	  42	25	0
#' # -100	40	1985	4	  14	20	  21	25	2.4
#' # -100	40	1985	4	  15	8.5	  40	17	0
#' # -100	40	1985	4	  16	6.5	  25	6	0
#' # -100	40	1985	4	  17	13	  34	24	0
#' 
#' ## (1) FWI System variables for a single weather station:
#' # Using the default initial values and batch argument, 
#' # the function calculate FWI variables chronically:
#' fwi.out1<-fwi(test_fwi) 				
#' # Using a different set of initial values:
#' fwi.out2<-fwi(test_fwi,init=data.frame(ffmc=80, dmc=10,dc=16, lat=50))
#' # This could also be done as the following:
#' fwi.out2<-fwi(test_fwi,init=data.frame(80,10,6,50))
#' # Or:
#' fwi.out2<-fwi(test_fwi,init=c(80,10,6,50))
#' # Latitude could be ignored, and the default value (55) will 
#' # be used:
#' fwi.out2<-fwi(test_fwi,init=data.frame(80,10,6))
#' 
#' ## (2) FWI for one or multiple stations in a single day:
#' # Change batch argument to FALSE, fwi calculates FWI 
#' # components based on previous day's fwi outputs:
#' 
#' fwi.out3<-fwi(test_fwi,init=fwi.out1,batch=FALSE)                 
#' # Using a suite of initials, assuming variables from fwi.out1
#' # are the initial values for different records. 
#' init_suite<-fwi.out1[,c("FFMC","DMC","DC","LAT")]
#' # Calculating FWI variables for one day but with multiple
#' # stations. Because the calculations is for one time step, 
#' # batch=FALSE:
#' fwi.out4<-fwi(test_fwi,init=init_suite,batch=FALSE)
#' 
#' ## (3) FWI for multiple weather stations over a period of time: 
#' #Assuming there are 4 weather stations in the test dataset, and they are 
#' # ordered by day:
#' test_fwi$day<-rep(1:(nrow(test_fwi)/4),each=4)
#' test_fwi$id<-rep(1:4,length(unique(test_fwi$day)))
#' # Running the function with the same default initial inputs, will receive a 
#' # warning message, but that is fine: 
#' fwi(test_fwi)
#' 
#' ## (4) Daylength adjustment:
#' # Change latitude values where the monthly daylength adjustments
#' are different from the standard ones
#' test_fwi$lat<-22
#' # With daylength adjustment
#' fwi(test_fwi)[1:3,]
#' # Without daylength adjustment
#' fwi(test_fwi,lat.adjust=FALSE)[1:3,]
#' 
#' @export fwi
#' 
fwi <- function(input, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55),
              batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = TRUE) {
  #############################################################################
  # Description: Canadian Forest Fire Weather Index Calculations. All code
  #              is based on a C code library that was written by Canadian
  #              Forest Service Employees, which was originally based on
  #              the Fortran code listed in the reference below. All equations
  #              in this code refer to that document, unless otherwise noted.
  #
  #              Equations and FORTRAN program for the Canadian Forest Fire 
  #              Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
  #              Canadian Forestry Service, Petawawa National Forestry 
  #              Institute, Chalk River, Ontario. Forestry Technical Report 33. 
  #              18 p.
  #
  #              Additional reference on FWI system
  #
  #              Development and structure of the Canadian Forest Fire Weather 
  #              Index System. 1987. Van Wagner, C.E. Canadian Forestry Service,
  #              Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
  #  
  #Args:  input:    View Documentation (fwi.Rd) for full description
  #                 of input data frame
  #       init:     Initializing moisture values
  #                 ffmc:     Fine Fuel Moisture Code (default 85)
  #                 dmc:      Duff Moisture Code (default 6)
  #                 dc:       Drought Code (default 15)
  #                 lat:      Latitude (decimal degrees, default 55)
  #       batch:    Function can be run in a batch mode, where multiple 
  #                 weather stations or points can be calculated at once. 
  #                 (TRUE/FALSE, default TRUE)
  #       out:      Display the calculated FWI values, with or without the 
  #                 inputs. (all/fwi, default all)
  #       lat.adjust: Option to adjust day length in the calculations 
  #                   (TRUE/FALSE, default TRUE)
  #       uppercase:  Output names in upper or lower case - a commonly 
  #                   asked for feature, as dataset naming conventions vary 
  #                   considerably. (TRUE/FALSE, default TRUE)
  #       
  #
  # Returns: A data.frame of the calculated FWI values with or without
  #          the input data attached to it.
  #
  #############################################################################
  
  #Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  names(input) <- tolower(names(input))
  
  #convert vector to data.frame to ensure consitency
  if (is.vector(init)){
    init <- as.data.frame(t(init))
  }
  names(init) <- tolower(names(init))
  #resolve missing names of the initializing variables if necessary
  if(substr(names(init), 1, 1)[1] == "x" | substr(names(init), 1, 1)[1] == "v"){
    if (ncol(init) == 3){
      names(init) <- c("ffmc", "dmc", "dc")
      init$lat <- 55
    }else if(ncol(init) == 4){
      names(init) <- c("ffmc", "dmc", "dc", "lat")
    }
  }
    
  #############################################################################
  #                                 
  # Set local variables and display warnings to user if default is being used
  #############################################################################
  ffmc_yda <- init$ffmc
  dmc_yda  <- init$dmc
  dc_yda   <- init$dc

  if ("lat" %in% names(input)) {
    lat <- input$lat
  }
  else {
    warning("latitude was not provided, assign default value 55")
    lat <- rep(55, nrow(input))
  }
  if ("long" %in% names(input)) {
    long <- input$long
  }
  else {
    warning("long was not provided, assign a default number -120")
    long <- rep(-120, nrow(input))
  }
  if ("yr" %in% names(input)) {
    yr <- as.numeric(as.character(input$yr))
  }
  else {
    warning("Year was not provided, assigned default number 5000")
    yr <- rep(5000, nrow(input))
  }
  if ("mon" %in% names(input)) {
    mon <- as.numeric(as.character(input$mon))
  }
  else {
    warning("Month was not provided, assigned the default value, July")
    mon <- rep(7, nrow(input))
  }
  if ("day" %in% names(input)) {
    day <- as.numeric(as.character(input$day))
  }
  else {
    warning("Day was not provided, assigned default number -99")
    day <- rep(-99, nrow(input))
  }

  #If batch selected, then sort the data by Date and id and determine the 
  # length of each run.
  # Currently when running multiple stations, the stations much have the same
  # amount of data and same start/end dates
  #Function stops running if these requirements are not met
  if (batch){
    if ("id" %in% names(input)) {
      input <- input[with(input,order(yr,mon,day,id)),]
      #number of stations
      n <- length(unique(input$id))
      if(length(unique(input[1:n, "id"])) != n){
        stop("Multiple stations have to start and end at the same dates, and 
             input data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  }else{
    n <- nrow(input)
  }

  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  if (!exists("temp") | is.null(temp)) 
    stop("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    stop("precipitation (prec) is missing!")
  if (length(prec[prec < 0]) > 0)
    stop("precipiation (prec) cannot be negative!")
  if (!exists("ws") | is.null(ws)) 
    stop("wind speed (ws) is missing!")
  if (length(ws[ws < 0]) > 0)
    stop("wind speed (ws) cannot be negative!")
  if (!exists("rh") | is.null(rh)) 
    stop("relative humidity (rh) is missing!")
  if (length(rh[rh < 0]) > 0)
    stop("relative humidity (rh) cannot be negative!")
  #############################################################################
  #                                 END
  # Set local variables and display warnings to user if default is being used
  #############################################################################

  if (length(temp) %% n != 0)
    warning("Missing records may generate wrong outputs")
  if (nrow(init) == 1 & n > 1){
    warning("Same initial data were used for multiple weather stations")
    ffmc_yda <- rep(ffmc_yda, n)
    dmc_yda <- rep(dmc_yda, n)
    dc_yda <- rep(dc_yda, n)
  }
  #if the number of rows in the init file does not equal that of the number of
  # stations, then stop execution as we do not have a complete input set
  if(nrow(init) > 1 & nrow(init) != n) {
    stop("Number of initial values do not match with number of weather 
         stations")
  }
  
  #Length of weather run
  n0 <- length(temp) / n
  #Initialize variables
  ffmc <- dmc <- dc <- isi <- bui <- fwi <- dsr <- NULL
  #For each day in the run
  for (i in 1:n0){
    #k is the data for all stations by day
    k  <- ((i - 1) * n + 1):(i * n)
    #constrain relative humidity
    rh[k] <- ifelse(rh[k] >= 100, 99.9999, rh[k])
    ###########################################################################
    # Fine Fuel Moisture Code (FFMC)
    ###########################################################################
    ffmc1 = .ffmcCalc(ffmc_yda, temp[k], rh[k], ws[k], prec[k])
    
    ###########################################################################
    # Duff Moisture Code (DMC)
    ###########################################################################
    dmc1 = .dmcCalc(dmc_yda, temp[k], rh[k], prec[k], lat[k], mon[k], 
                    lat.adjust)
    
    ###########################################################################
    # Drought Code (DC)
    ###########################################################################
    dc1 <- .dcCalc(dc_yda, temp[k], rh[k], prec[k], lat[k], mon[k],
                   lat.adjust)
    
    ###########################################################################
    # Initial Spread Index (ISI)
    ###########################################################################
    isi1 <- .ISIcalc(ffmc1, ws[k], FALSE)
    
    ###########################################################################
    # Buildup Index (BUI)
    ###########################################################################
    bui1 <- .buiCalc(dmc1, dc1)
    
    ###########################################################################
    # Fire Weather Index (FWI)
    ###########################################################################
    fwi1 <- .fwiCalc(isi1, bui1)
    ###########################################################################
    #                   Daily Severity Rating (DSR)
    ###########################################################################
    #Eq. 31
    dsr1 <- 0.0272 * (fwi1^1.77)
    
    #Concatenate values
    ffmc<-c(ffmc,ffmc1)
    dmc<-c(dmc,dmc1)
    dc<-c(dc,dc1)
    isi<-c(isi,isi1)
    bui<-c(bui,bui1)
    fwi<-c(fwi,fwi1)
    dsr<-c(dsr,dsr1)
    ffmc_yda<-ffmc1
    dmc_yda<-dmc1
    dc_yda<-dc1
  } 
  
  #If output specified is "fwi", then return only the FWI variables
  if (out == "fwi") {
    new_FWI <- data.frame(ffmc = ffmc, dmc = dmc, dc = dc, isi = isi, 
                          bui = bui, fwi = fwi, dsr = dsr)
    if (uppercase){
      names(new_FWI) <- toupper(names(new_FWI))
    }
  }
  #If output specified is "all", then return both FWI and input weather vars
  else {
    if (out == "all") {
      new_FWI <- cbind(input, ffmc, dmc, dc, isi, bui, fwi, dsr)
      if (uppercase){
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(new_FWI)
}
