#' @title Grass Fuel Moisture Raster Calculation
#'
#' @description Calculation of the Grass Fuel Moisture Code. This calculates the
#' moisture content of both the surface of a fully cured matted grass layer and
#' also an equivalent Grass Fuel Moisture Code. All equations come from Wotton
#' (2009) as cited below unless otherwise specified.
#'
#' @references
#' Wotton, B.M. 2009. A grass moisture model for the Canadian
#' Forest Fire Danger Rating System. In: Proceedings 8th Fire and
#' Forest Meteorology Symposium, Kalispell, MT Oct 13-15, 2009.
#' Paper 3-2. \url{https://ams.confex.com/ams/pdfpapers/155930.pdf}
#'
#' @param input [SpatRast stack]
#' \tabular{lll}{
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh}   \tab (required) \tab Relative humidity (\%)\cr
#' \var{ws}   \tab (required) \tab 10-m height wind speed (km/h)\cr
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr
#' \var{isol} \tab (required) \tab Solar radiation (kW/m^2)\cr }
#' @param GFMCold    GFMC from yesterday (double, default=85)
#' @param time.step  The hourly time steps (integer hour, default=1)
#' @param roFL       Nominal fuel load of the fine fuel layer
#'    (kg/m^2 double, default=0.3)
#' @param out        Output format (GFMCandMC/MC/GFMC/ALL, default=GFMCandMC)
#'
#' @return Returns a spatrast stack of either MC, GFMC, GFMC and MC or All
#'
#' @importFrom terra rast lapp
#' @importFrom data.table data.table
#'
#' @export gfmcRaster
#'
#' @examples
#' set.seed(5123)
#' test_gfmc_r <- rast(
#'   nrows = 25,
#'   ncols = 25,
#'   crs = "EPSG:3402",
#'   resolution = 100,
#'   ymin = 5652012,
#'   ymax = 5652012 + (25 * 100),
#'   xmin = 565550,
#'   xmax = 565550 + (25 * 100),
#'   names = "temp",
#'   vals = sample(x = 19:27, size = 25 * 25, replace = TRUE)
#' )
#'
#' test_gfmc_r <- c(
#'   test_gfmc_r,
#'   setValues(test_gfmc_r, sample(x = 0:3, size = 25 * 25, replace = TRUE)),
#'   setValues(test_gfmc_r, sample(x = 10:20, size = 25 * 25, replace = TRUE)),
#'   setValues(test_gfmc_r, sample(x = 30:70, size = 25 * 25, replace = TRUE)),
#'   setValues(
#'     test_gfmc_r,
#'     sample(
#'       x = (5:950) / 1000,
#'       size = 25 * 25,
#'       replace = TRUE
#'     )
#'   )
#' )
#' names(test_gfmc_r) <- c("temp", "prec", "ws", "rh", "isol")
#'
#' gfmcRaster(test_gfmc_r, out = "GFMCandMC")
gfmcRaster <- function(
    input,
    GFMCold = 85,
    time.step = 1,
    roFL = 0.3,
    out = "GFMCandMC") {
  if (class(input) != "SpatRaster") {
    input <- terra::rast(input)
  }
  names(input) <- tolower(names(input))
  out <- toupper(out)

  if (class(GFMCold) != "SpatRaster") {
    GFMCold <-setValues(input[[1]], GFMCold)
    }

  if (class(GFMCold) == "SpatRaster") {
    names(GFMCold) <- "GFMCold"
    }

  roFL <- if (typeof(roFL) == "double") setValues(input[[1]], roFL)
  if (class(roFL) == "SpatRaster") names(roFL) <- "roFL"

  # Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }

  # show warnings when inputs are missing
  required_cols <- data.table(
    full = c(
      "temperature", "precipitation", "wind speed", "relative humidity",
      "insolation"
    ),
    short = c("temp", "prec", "ws", "rh", "isol")
  )

  if (nrow(required_cols[-which(names(input) %in% short)]) > 0) {
    stop(
      paste(
        required_cols[-which(names(input) %in% short), full],
        collapse = ", "
      ),
      " is missing!"
    )
  }

  if (is.numeric(GFMCold) & length(GFMCold) == 1) {
    warning("Single GFMCold value for grid is applied to the whole grid")
    # GFMCold <- setValues(input["temp"], GFMCold)
  }
  if (is.numeric(time.step) & length(time.step) == 1) {
    warning("Single time.step value for grid is applied to the whole grid")
    # time.step <- setValues(input["temp"], time.step)
  }
  if (is.numeric(roFL) & length(roFL) == 1) {
    warning("Single roFL value for grid is applied to the whole grid")
    # roFL <- setValues(input["temp"], roFL)
  }
  validOutTypes <- toupper(c("GFMCandMC", "MC", "GFMC", "ALL"))
  out <- toupper(out)
  if (!(out %in% validOutTypes)) {
    stop(paste("'", out, "' is an invalid 'out' type.", sep = ""))
  }
  # get the length of the data stream
  mc.r <- lapp(
    x = c(
      input[[c("temp", "rh", "ws", "prec", "isol")]],
      GFMCold,
      roFL
    ),
    fun = Vectorize(grass_fuel_moisture),
    time.step = time.step,
    usenames = TRUE
  )
  names(mc.r) <- "MC"
  gfmc.r <- lapp(x = mc.r, fun = Vectorize(grass_fuel_moisture_code))
  names(gfmc.r) <- "GFMC"
  # Return requested 'out' type
  if (out == "ALL") {
    return(c(input, gfmc.r, mc.r))
  } else if (out == "GFMC") {
    return(gfmc.r)
  } else if (out == "MC") {
    return(mc.r)
  } else { # GFMCandMC
    return(c(gfmc.r, mc.r))
  }
}
