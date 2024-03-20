#' Raster-based Fire Weather Index System
#'
#' @description \code{fwiRaster} is used to calculate the outputs of the
#' Canadian Forest Fire Weather Index (FWI) System for one day based on noon
#' local standard time (LST) weather observations of temperature, relative
#' humidity, wind speed, and 24-hour rainfall, as well as the previous day's
#' fuel moisture conditions. This function takes rasterized input and generates
#' raster maps as outputs.
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
#' The first three outputs of the system (the Fire Fuel Moisture Code, the Duff
#' Moisture Code, and the Drought Code) track moisture in different layers of
#' the fuel making up the forest floor. Their calculation relies on the daily
#' fire weather observation and also, importantly, the code value from the
#' previous day as they are in essence bookkeeping systems tracking the amount
#' of moisture (water) in to and out of the layer.  It is therefore important
#' that when calculating FWI System outputs over an entire fire season, an
#' uninterrupted daily weather stream is provided; one day is the assumed time
#' step in the models and thus missing data must be filled in.
#'
#' The next three outputs of the System are relative (unitless) indicators of
#' aspects of fire behavior potential: spread rate (the Initial Spread Index),
#' fuel consumption (the Build-up Index) and fire intensity per unit length of
#' fire front (the Fire Weather Index).  This final index, the fwi, is the
#' component of the System used to establish the daily fire danger level for a
#' region and communicated to the public.  This final index can be transformed
#' to the Daily Severity Rating (dsr) to provide a more reasonably-scaled
#' estimate of fire control difficulty.
#'
#' Both the Duff Moisture Code (dmc) and Drought Code (dc) are influenced by
#' day length (see Van Wagner, 1987). Day length adjustments for different
#' ranges in latitude can be used (as described in Lawson and Armitage 2008
#' (\url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf})) and are included
#' in this R function; latitude must be positive in the northern hemisphere and
#' negative in the southern hemisphere.
#'
#' The default initial (i.e., "start-up") fuel moisture code values (FFMC=85,
#' DMC=6, DC=15) provide a reasonable set of conditions for most springtime
#' conditions in Canada, the Northern U.S., and Alaska. They are not suitable
#' for particularly dry winters and are presumably not appropriate for
#' different parts of the world.
#'
#' @param input A stack or brick containing rasterized daily weather
#' observations taken at noon LST. Variable names have to be the same as in the
#' following list, but they are case insensitive. The order in which the inputs
#' are entered is not important.
#'
#' \tabular{lll}{
#' \var{lat} \tab (recommended) \tab Latitude (decimal degree,
#' __default=55__)\cr
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr
#' \var{ws} \tab
#' (required) \tab 10-m height wind speed (km/h)\cr
#' \var{prec} \tab (required)
#' \tab 24-hour rainfall (mm)\cr }
#' @param init A vector that contains the initial values for FFMC, DMC, and DC
#' or a stack that contains raster maps of the three moisture codes calculated
#' for the previous day, which will be used for the current day's calculation.
#' Defaults are the standard initial values for FFMC, DMC, and DC defined as
#' the following:
#'
#' \tabular{lll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Default} \cr
#' \var{ffmc}
#'    \tab Previous day Fine Fuel Moisture Code (FFMC; unitless) \tab 85 \cr
#' \var{dmc} \tab Previous day Duff Moisture Code (DMC; unitless)\tab 6 \cr
#' \var{dc} \tab Previous Day Drought Code (DC; unitless) \tab 15\cr
#' \var{lat} \tab Latitude of the weather station (\emph{Optional})\tab 55 \cr}
#'
#' @param mon Month of the year (integer 1~12, default=7). Month is used in
#' latitude adjustment (\code{lat.adjust}), it is therefore recommended when
#' \code{lat.adjust=TRUE} was chosen.
#' @param out The function offers two output options, \code{out="all"} will
#' produce a raster stack include both the input and the FWI System outputs;
#' \code{out="fwi"} will generate a stack with only the FWI system components.
#' @param lat.adjust The function offers options for whether latitude
#' adjustments to day lengths should be applied to the calculations. The
#' default value is "TRUE".
#' @param uppercase Output in upper cases or lower cases would be decided by
#' this argument. Default is TRUE.
#' @return By default, \code{fwi} returns a raster stack which includes both
#' the input and the FWI System variables, as describe below: \item{Inputs
#' }{Including \code{temp}, \code{rh}, \code{ws}, and \code{prec} with
#' \code{lat} as optional.} \item{ffmc }{Fine Fuel Moisture Code} \item{dmc
#' }{Duff Moisture Code} \item{dc }{Drought Code} \item{isi }{Initial Spread
#' Index} \item{bui }{Buildup Index} \item{fwi }{Fire Weather Index} \item{dsr
#' }{Daily Severity Rating}
#'
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#'
#' @seealso \code{\link{fbp}}, \code{\link{fbpRaster}}, \code{\link{fwi}},
#' \code{\link{hffmc}}, \code{\link{hffmcRaster}}
#'
#' @references 1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and
#' FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For.
#' Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.
#' \url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf}
#'
#' 2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest
#' fire weather index system. Forest Technology Report 35. (Canadian Forestry
#' Service: Ottawa). \url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf}
#'
#' 3.  Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian
#' Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North.
#' For. Cent., Edmonton, AB.
#' \url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#'
#' @keywords methods
#'
#' @examples
#'
#' library(cffdrs)
#' require(terra)
#' # The test data is a stack with four input variables including
#' # daily noon temp, rh, ws, and prec (we recommend tif format):
#' day01src <- rast(
#'   system.file("extdata", "test_rast_day01.tif", package = "cffdrs")
#' )
#' day01 <- crop(day01src, c(250, 255, 47, 51))
#' # assign variable names:
#' names(day01) <- c("temp", "rh", "ws", "prec")
#' # (1) use the initial values
#' foo <- fwiRaster(day01)
#' plot(foo)
#' ### Additional, longer running examples ###
#' # (2) use initial values with larger raster
#' day01 <- day01src
#' names(day01) <- c("temp", "rh", "ws", "prec")

#' \donttest{
#' foo <- fwiRaster(day01)
#' }
#' plot(foo)
#'
#' @export fwiRaster
fwiRaster <- function(
    input,
    init = c(ffmc = 85, dmc = 6, dc = 15),
    mon = 7,
    out = "all",
    lat.adjust = TRUE,
    uppercase = TRUE) {
  # due to NSE notes in R CMD check
  short = full = NULL
  # Reference latitude for DMC day length adjustment
  # 46N: Canadian standard, latitude >= 30N   (Van Wagner 1987)
  ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  # 20N: For 30 > latitude >= 10
  ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1, 8.6, 8.1, 7.8)
  # 20S: For -10 > latitude >= -30
  ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
  # 40S: For -30 > latitude
  ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
  # For latitude near the equator, we simple use a factor of 9 for all months

  # Day length factor for DC Calculations
  # 20N: North of 20 degrees N
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  # 20S: South of 20 degrees S
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
  # Near the equator, we just use 1.4 for all months.

  # Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  if (!is(input,"SpatRaster")) {
    input <- terra::rast(input)
  }
  names(input) <- tolower(names(input))
  had_latitude <- "lat" %in% names(input)
  if (!had_latitude) {
    # FIX: use actual latitude from raster
    # input[["lat"]] <- terra::init(input[["temp"]], "y")
    # use old default value
    input[["lat"]] <- terra::init(input[["temp"]], 55)
  }


  required_cols <- data.table(
    full = c("temperature", "precipitation", "wind speed", "relative humidity"),
    short = c("temp", "prec", "ws", "rh")
  )

  if (nrow(required_cols[-which(names(input) %in% short)]) > 0) {
    stop(
      paste(
        required_cols[-which(names(input) %in% short), full],
        collapse = " , "
      ),
      " is missing!"
    )
  }

  if (!length(input[["prec"]][input[["prec"]] < 0]) == 0) {
    stop("precipiation (prec) cannot be negative!")
  }
  if (!length(input[["ws"]][input[["ws"]] < 0]) == 0) {
    stop("wind speed (ws) cannot be negative!")
  }
  if (!length(input[["rh"]][input[["rh"]] < 0]) == 0) {
    stop("relative humidity (rh) cannot be negative!")
  }

  names(init) <- tolower(names(init))

  # Assign values for initializing variables
  if (is.numeric(init)) {
    if (is.null(names(init))) {
      names(init) <- c("ffmc", "dmc", "dc")
    }
    ffmc_yda <- dmc_yda <- dc_yda <- input[["temp"]]
    terra::values(ffmc_yda) <- init[["ffmc"]]
    names(ffmc_yda) <- "ffmc_yda"
    terra::values(dmc_yda) <- init[["dmc"]]
    terra::values(dc_yda) <- init[["dc"]]
  } else {
    ffmc_yda <- init$ffmc
    dmc_yda <- init$dmc
    dc_yda <- init$dc
  }
  # constrain relative humidity
  was_rh_100 <- input[["rh"]] >= 100
  input[["rh"]][was_rh_100] <- 99.9999
  ###########################################################################
  #                    Fine Fuel Moisture Code (FFMC)
  ###########################################################################

  ffmc <- lapp(
    x = c(ffmc_yda, input[[c("temp", "rh", "ws", "prec")]]),
    fun = Vectorize(fine_fuel_moisture_code)
  )

  ###########################################################################
  #                        Duff Moisture Code (DMC)
  ###########################################################################

  dmc <- lapp(
    x = c(
      dmc_yda,
      input[[c("temp", "rh", "prec")]],
      input[["lat"]],
      setValues(input[["temp"]], mon)
    ),
    fun = Vectorize(duff_moisture_code),
    lat.adjust = lat.adjust
  )

  ###########################################################################
  #                             Drought Code (DC)
  ###########################################################################

  dc <- lapp(
    x = c(
      dc_yda,
      input[[c("temp", "rh", "prec")]],
      input[["lat"]],
      setValues(input[["temp"]], mon)
    ),
    fun = Vectorize(drought_code),
    lat.adjust = lat.adjust
  )

  ###########################################################################
  #                    Initial Spread Index (ISI)
  ###########################################################################

  isi <- lapp(
    x = c(ffmc, input[["ws"]]),
    fun = Vectorize(initial_spread_index),
    fbpMod = FALSE
  )

  ###########################################################################
  #                       Buildup Index (BUI)
  ###########################################################################

  bui <- lapp(x = c(dmc, dc), fun = Vectorize(buildup_index))

  ###########################################################################
  #                     Fire Weather Index (FWI)
  ###########################################################################

  fwi <- lapp(x = c(isi, bui), fun = Vectorize(fire_weather_index))

  ###########################################################################
  #                   Daily Severity Rating (DSR)
  ###########################################################################
  # Eq. 31
  dsr <- daily_severity_rating(fwi)
  if (!had_latitude) {
    input <- input[[setdiff(names(input), c("lat"))]]
  }
  input[["rh"]][was_rh_100] <- 100.0
  # If output specified is "fwi", then return only the FWI variables
  if (out == "fwi") {
    # Creating a raster stack of FWI variables to return
    new_FWI <- c(ffmc, dmc, dc, isi, bui, fwi, dsr)
    names(new_FWI) <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr")
    if (uppercase) {
      names(new_FWI) <- toupper(names(new_FWI))
    }
    # If output specified is "all", then return both FWI and input weather vars
  } else {
    if (out == "all") {
      # Create a raster stack of input and FWI variables
      new_FWI <- c(input, ffmc, dmc, dc, isi, bui, fwi, dsr)
      names(new_FWI) <- c(
        names(input),
        "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr"
      )
      if (uppercase) {
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(new_FWI)
}
