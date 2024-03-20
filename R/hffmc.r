#' Hourly Fine Fuel Moisture Code
#'
#' @description \code{hffmc} is used to calculate hourly Fine Fuel Moisture Code
#' (FFMC) and is based on a calculation routine first described in detail by Van
#' Wagner (1977) and which has been updated in minor ways by the Canadian Forest
#' Service to have it agree with the calculation methodology for the daily FFMC
#' (see \code{\link{fwi}}).  In its simplest typical use this current routine
#' calculates a value of FFMC based on a series of uninterrupted hourly weather
#' observations of screen level (~1.4 m) temperature, relative humidity, 10 m
#' wind speed, and 1-hour rainfall. This implementation of the function
#' includes an optional time.step input which is defaulted to one hour, but can
#' be reduced if sub-hourly calculation of the code is needed.  The FFMC is in
#' essence a bookkeeping system for moisture content and thus it needs to use
#' the last time.step's value of FFMC in its calculation as well.  This
#' function could be used for either one weather station or for multiple
#' weather stations.
#'
#' The hourly FFMC is very similar in its structure and calculation to the
#' Canadian Forest Fire Weather Index System's daily FFMC (\code{\link{fwi}})
#' but has an altered drying and wetting rate which more realistically reflects
#' the drying and wetting of a pine needle litter layer sitting on a decaying
#' organic layer.  This particular implementation of the Canadian Forest Fire
#' Danger Rating System's hourly FFMC provides for a flexible time step; that
#' is, the data need not necessarily be in time increments of one hour.  This
#' flexibility has been added for some users who use this method with data
#' sampled more frequently that one hour.  We do not recommend using a time
#' step much greater than one hour. An important and implicit assumption in
#' this calculation is that the input weather is constant over the time step of
#' each calculation (e.g., typically over the previous hour).  This is a
#' reasonable assumption for an hour; however it can become problematic for
#' longer periods.  For brevity we have referred to this routine throughout
#' this description as the hourly FFMC.
#'
#' Because of the shortened time step, which can lead to more frequent
#' calculations and conversion between moisture content and the code value
#' itself, we have increased the precision of one of the constants in the
#' simple formula that converts litter moisture content to the 'Code' value.
#' This is necessary to avoid a potential bias that gets introduced during
#' extremely dry conditions.  This is simply a change in the precision at which
#' this constant is used in the equation and is not a change to the standard
#' FFMC conversion between moisture and code value (which is referred to as the
#' FF-scale).
#'
#' The calculation requires the previous hour's FFMC as an input to the
#' calculation of the current hour's FFMC; this is because the routine can be
#' thought of as a bookkeeping system and needs to know the amount of moisture
#' being held in the fuel prior to any drying or wetting in the current period.
#' After each hour's calculation that newly calculated FFMC simply becomes the
#' starting FFMC in the next hour's calculation.  At the beginning of the
#' calculations at a station this previous hours FFMC must be estimated. It is
#' typical to use a value of 85 when this value cannot be estimated more
#' accurately; this code value corresponds to a moisture content of about 16\%
#' in typical pine litter fuels.
#'
#' Typically this dataframe also contains date and
#' hour fields so outputs can be associated with a specific day and time,
#' however these fields are not used in the calculations.  If multiple weather
#' stations are being used, a weather station ID field is typically included as
#' well, though this is simply for bookkeeping purposes and does not affect the
#' calculation.
#'
#' @param input A dataframe containing input variables of hourly
#' weather observations. It is important that variable names have to be the
#' same as in the following list, but they are case insensitive. The order in
#' which the input variables are entered is not important.
#'
#' \tabular{lll}{
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr
#' \var{hr} \tab (optional) \tab Hourly value to calculate sub-hourly ffmc \cr
#' \var{bui} \tab (optional) \tab Daily BUI value for the computation of hourly
#' FWI. \cr
#' \tab\tab It is required when \code{hourlyFWI=TRUE}.\cr }
#' @param ffmc_old Initial FFMC. At the start of calculations at a particular
#' station there is a need to provide an estimate of the FFMC in the previous
#' timestep; this is because the FFMC is, in essence, a bookkeeping system for
#' moisture.  If no estimate of previous hour's FFMC is available the function
#' will use default value, \code{ffmc_old=85}. When using the routine to
#' calculate hourly FFMC at multiple stations the \code{ffmc_old} argument can
#' also accept a vector with the same number of weather stations.
#' @param time.step Is the time (in hours) between the previous value of FFMC
#' and the current time at which we want to calculate a new value of the FFMC.
#' When not specified it will take on a default value of \code{time.step=1}.
#' @param calc.step Optional for whether time step between two observations is
#' calculated. Default is FALSE, no calculations. This is used when time
#' intervals are not uniform in the input.
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate hourly or
#' sub-hourly FFMC for one weather station over a period of time iteratively.
#' If multiple weather stations are processed, an additional "id" column is
#' required in the input weatherstream to label different stations, and the
#' data needs to be sorted by date/time and "id".  If \code{batch=FALSE}, the
#' function calculates only one time step base on either the previous hourly
#' FFMC or the initial start value.
#' @param hourlyFWI Optional for the computation of hourly ISI, FWI, and DSR.
#' Default is FALSE. While \code{hourlyFWI=TRUE}, daily BUI is required for the
#' computation of FWI.
#' @return \code{hffmc} returns a vector of hourly or sub-hourly FFMC values,
#' which may contain 1 or multiple elements. Optionally when
#' \code{hourlyFWI=TRUE}, the function also output a data.frame contains input
#' weatherstream as well as the hourly or sub-hourly FFMC, ISI, FWI, and DSR.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, Brett Moore, and Mike
#' Flannigan
#' @seealso \code{\link{fbp}}, \code{\link{fwi}}, \code{\link{hffmcRaster}}
#' @references Van Wagner, C.E. 1977. A method of computing fine fuel moisture
#' content throughout the diurnal cycle. Environment Canada, Canadian Forestry
#' Service, Petawawa Forest Experiment Station, Chalk River, Ontario.
#' Information Report PS-X-69.
#' \url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/25591.pdf}
#' @keywords methods
#' @examples
#'
#' library(cffdrs)
#' data("test_hffmc")
#' # show the data format:
#' head(test_hffmc)
#' # (1)hffmc default:
#' # Re-order the data by year, month, day, and hour:
#' test_hffmc <- test_hffmc[with(test_hffmc, order(yr, mon, day, hr)), ]
#' # Because the test data has 24 hours input variables
#' # it is possible to calculate the hourly FFMC chronically
#' # through multiple days(with the default initial ffmc_old=85):
#' test_hffmc$ffmc_default <- hffmc(test_hffmc)
#' # (2) Calculate FFMC for multiple stations:
#' # Calculate hourly FFMC with only one initial
#' # value (ffmc_old=85), but multiple weather stations.
#' # Sort the input by date/time and the station id:
#' test_hffmc <- test_hffmc[with(test_hffmc, order(yr, mon, hr)), ]
#' # Add weather station id:
#' test_hffmc$id <- rep(1:10, nrow(test_hffmc) / 10)
#' # check the data:
#' head(test_hffmc)
#' test_hffmc$ffmc01 <- hffmc(test_hffmc, batch = TRUE)
#' # With multiple initial FFMC (ffmc_old) as a vector:
#' test_hffmc$ffmc02 <- hffmc(
#'   test_hffmc,
#'   ffmc_old = sample(70:100, 10, replace = TRUE),
#'   batch = TRUE
#' )
#' # One time step assuming all records are from different
#' # weather stations:
#' foo <- hffmc(test_hffmc, batch = FALSE)
#' # (3) output all hourly FWI System variables:
#' test_hffmc$id <- NULL
#' test_hffmc <- test_hffmc[with(test_hffmc, order(yr, mon, day, hr)), ]
#' foo <- hffmc(test_hffmc, hourlyFWI = TRUE)
#' # this will not run: warning message requesting for daily BUI
#' test_hffmc$bui <- 100
#' foo <- hffmc(test_hffmc, hourlyFWI = TRUE)
#' # (4) Calculate time steps in case the time intervals are
#' # not uniform:
#' dat0 <- test_hffmc[sample(1:30, 20), ]
#' dat0 <- dat0[with(dat0, order(yr, mon, day, hr)), ]
#' # with or without calc.step, hffmc is going to generate
#' # different FFMC values.
#' # without calculating time step (default):
#' hffmc(dat0, time.step = 1)
#' # with calc.step=TRUE, time.step=1 is applied to
#' # only the first record, the rests would be calculated:
#' hffmc(dat0, time.step = 1, calc.step = TRUE)
#'
#' @export hffmc
hffmc <- function(
    input,
    ffmc_old = 85,
    time.step = 1,
    calc.step = FALSE,
    batch = TRUE,
    hourlyFWI = FALSE) {
  # due to NSE notes in R CMD check
  short = full = NULL
  t0 <- time.step
  names(input) <- tolower(names(input))
  # set up number of stations
  if (batch) {
    if ("id" %in% names(input)) {
      n <- length(unique(input$id))
      if (length(unique(input[1:n, "id"])) != n) {
        stop(paste0(
          "Multiple stations have to start and end at the same dates/time,",
          " and the data must be sorted by date/time and id"
        ))
      }
    } else {
      n <- 1
    }
  } else {
    n <- nrow(input)
  }
  Fo <- ifelse(length(ffmc_old) == 1 & n > 1, rep(ffmc_old, n), ffmc_old)
  # Check that the parameters are correct
  if (calc.step) {
    hr <- input$hr
    if (!exists("hr") | is.null(hr)) {
      warning("hour value is missing!")
    }
  }
  required_cols <- data.table(
    full = c("temperature", "precipitation", "wind speed", "relative humidity"),
    short = c("temp", "prec", "ws", "rh")
  )

  if (nrow(required_cols[-which(short %in% names(input))]) > 0) {
    stop(
      paste(
        required_cols[-which(short %in% names(input)), full],
        collapse = " , "
      ),
      " is missing!"
    )
  }

  if (!length(input[["prec"]][input[["prec"]] < 0]) == 0) {
    warning("precipiation (prec) cannot be negative!")
  }
  if (!length(input[["ws"]][input[["ws"]] < 0]) == 0) {
    warning("wind speed (ws) cannot be negative!")
  }
  if (!length(input[["rh"]][input[["rh"]] < 0]) == 0) {
    warning("relative humidity (rh) cannot be negative!")
  }
  if (length(input[["rh"]]) %% n != 0) {
    warning("input do not match with number of weather stations")
  }
  # Length of weather run
  n0 <- length(input[["rh"]]) / n
  f <- NULL
  # For each day in the run
  for (i in 1:n0) {
    # k is the data for all stations by day
    k <- ((i - 1) * n + 1):(i * n)
    if (calc.step & i > 1) {
      t0 <- ifelse(n0 > 1, hr[k] - hr[k - n], t0)
      t0 <- ifelse(t0 == -23, 1, t0)
      t0 <- ifelse(t0 < 0, -1 * t0, t0)
    }
    f1 <- hourly_fine_fuel_moisture_code(
      temp = input$temp[k],
      ws = input$ws[k],
      rh = input$rh[k],
      prec = input$prec[k],
      Fo = Fo,
      t0 = t0
    )
    Fo <- f1
    f <- c(f, Fo)
  }
  # Calculate hourly isi and fwi
  if (hourlyFWI) {
    bui <- input$bui
    ws <- input$ws
    if (!exists("bui") | is.null(bui)) {
      warning("Daily BUI is required to calculate hourly FWI")
    } else {
      # Calculate ISI
      isi <- initial_spread_index(f, ws, FALSE)
      # Calculate FWI
      fwi <- fire_weather_index(isi, bui)
      # Calculate DSR
      dsr <- daily_severity_rating(fwi)
      # Put all data into a data.frame to return
      output <- cbind(
        input,
        data.frame(ffmc = f, isi = isi, fwi = fwi, dsr = dsr)
      )
      return(output)
    }
    # otherwise just return hffmc
  } else {
    return(f)
  }
}
