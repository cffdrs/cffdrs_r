# used in conversion between FFMC and moisture content
FFMC_COEFFICIENT <- 250.0 * 59.5 / 101.0

#' Fine Fuel Moisture Code Calculation
#'
#' @description Fine Fuel Moisture Code Calculation. All code is based on a C
#' code library  that was written by Canadian Forest Service Employees, which
#' was originally  based on the Fortran code listed in the reference below. All
#' equations in this code refer to that document.
#'
#' Equations and FORTRAN program for the Canadian Forest Fire Weather Index
#' System. 1985. Van Wagner, C.E.; Pickett, T.L. Canadian Forestry Service,
#' Petawawa National Forestry Institute, Chalk River, Ontario. Forestry
#' Technical Report 33. 18 p.
#'
#' Additional reference on FWI system Development and structure of the Canadian
#' Forest Fire Weather Index System. 1987. Van Wagner, C.E. Canadian Forestry
#' Service, Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
#'
#'
#' @param ffmc_yda   The Fine Fuel Moisture Code from previous iteration
#' @param temp       Temperature (centigrade)
#' @param rh         Relative Humidity (%)
#' @param prec       Precipitation (mm)
#' @param ws         Wind speed (km/h)
#'
#' @return A single fine fuel moisture code value
#' @noRd

fine_fuel_moisture_code <- function(ffmc_yda, temp, rh, ws, prec) {
  # Eq. 1
  wmo <- FFMC_COEFFICIENT * (101 - ffmc_yda) / (59.5 + ffmc_yda)
  # Eq. 2 Rain reduction to allow for loss in
  #  overhead canopy
  ra <- prec
  ra[prec > 0.5] <- prec[prec > 0.5] - 0.5
  # Eqs. 3a & 3b
  wmo.sel <- prec > 0.5 & wmo > 150 
  wmo[wmo.sel] <- (wmo[wmo.sel] + 0.0015 * (wmo[wmo.sel] - 150) * (wmo[wmo.sel] - 150) * sqrt(ra[wmo.sel]) + 
                     42.5 * ra[wmo.sel] * exp(-100 / (251 - wmo[wmo.sel])) * (1 - exp(-6.93 / ra[wmo.sel])))
  wmo.sel <- prec > 0.5 & wmo <= 150
  wmo[wmo.sel] <- wmo[wmo.sel] + 42.5 * ra[wmo.sel] * exp(-100 / (251 - wmo[wmo.sel])) * (1 - exp(-6.93 / ra[wmo.sel]))
  # The real moisture content of pine litter ranges up to about 250 percent,
  # so we cap it at 250
  wmo[wmo > 250] <- 250
  # Eq. 4 Equilibrium moisture content from drying
  ed <- (0.942 * (rh^0.679) + (11 * exp((rh - 100) / 10))
    + 0.18 * (21.1 - temp) * (1 - 1 / exp(rh * 0.115)))
  # Eq. 5 Equilibrium moisture content from wetting
  ew <- (0.618 * (rh^0.753) + (10 * exp((rh - 100) / 10))
    + 0.18 * (21.1 - temp) * (1 - 1 / exp(rh * 0.115)))
  # Eq. 6a (ko) Log drying rate at the normal temperature of 21.1 C
  z <- rep(0.0, length(wmo))
  z.sel <- wmo < ed & wmo < ew
  z[z.sel] <- (0.424 * (1 - (((100 - rh[z.sel]) / 100)^1.7))
               + 0.0694 * sqrt(ws[z.sel]) * (1 - ((100 - rh[z.sel]) / 100)^8))
  # Eq. 6b Affect of temperature on  drying rate
  x <- z * 0.581 * exp(0.0365 * temp)
  # Eq. 8
  wm <- wmo
  wm.sel <- wmo < ed & wmo < ew
  wm[wm.sel] <- ew[wm.sel] - (ew[wm.sel] - wmo[wm.sel]) / (10^x[wm.sel])
  # Eq. 7a (ko) Log wetting rate at the normal temperature of 21.1 C
  z.sel <- wmo > ed
  z[z.sel] <- (0.424 * (1 - (rh[z.sel] / 100)^1.7) + 0.0694 * sqrt(ws[z.sel]) * (1 - (rh[z.sel] / 100)^8))
  # Eq. 7b Affect of temperature on  wetting rate
  x <- z * 0.581 * exp(0.0365 * temp)
  # Eq. 9
  wm[wmo > ed] <- ed[wmo > ed] + (wmo[wmo > ed] - ed[wmo > ed]) / (10^x[wmo > ed])
  wm[is.na(wmo > ed)] <- NA
  # Eq. 10 Final ffmc calculation
  ffmc1 <- (59.5 * (250 - wm)) / (FFMC_COEFFICIENT + wm)
  # Constraints
  ffmc1[ffmc1 > 101] <- 101
  ffmc1[ffmc1 < 0] <- 0
  return(ffmc1)
}

.ffmcCalc <- function(...) {
  .Deprecated("fine_fuel_moisture_code")
  return(fine_fuel_moisture_code(...))
}
