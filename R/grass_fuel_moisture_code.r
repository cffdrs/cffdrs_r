#' Grass Fuel Moisture Calculation
#'
#' @description This is the actual calculation for grass fuel moisture
#'
#' @param MC0 An output from the mcCalc functions
#'
#' @seealso mcCalc
#'
#' @return \code{gfmc}
#'
#' @export grass_fuel_moisture_code

grass_fuel_moisture_code <- function(MC0) {
  # Eq. 12 - Calculate GFMC
  GFMC0 <- 59.5 * ((250 - MC0) / (FFMC_COEFFICIENT + MC0))
  return(GFMC0)
}
