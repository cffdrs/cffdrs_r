#' Duff Moisture Code Calculator
#'
#' @description Duff Moisture Code Calculation. All code is based on a C code
#' library that was written by Canadian Forest Service Employees, which was
#' originally based on the Fortran code listed in the reference below. All
#' equations in this code refer to that document.
#'
#' Equations and FORTRAN program for the Canadian Forest Fire Weather Index
#' System. 1985. Van Wagner, C.E.; Pickett, T.L. Canadian Forestry Service,
#' Petawawa National Forestry Institute, Chalk River, Ontario. Forestry
#' Technical Report 33. 18 p.
#'
#' Additional reference on FWI system
#'
#' Development and structure of the Canadian Forest Fire Weather Index System.
#' 1987. Van Wagner, C.E. Canadian Forestry Service, Headquarters, Ottawa.
#' Forestry Technical Report 35. 35 p.
#'
#' @param dmc_yda    The Duff Moisture Code from previous iteration
#' @param temp       Temperature (centigrade)
#' @param rh         Relative Humidity (%)
#' @param prec       Precipitation(mm)
#' @param lat        Latitude (decimal degrees)
#' @param mon        Month (1-12)
#' @param lat.adjust Latitude adjustment (TRUE, FALSE, default=TRUE)
#'
#' @references \url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf}
#' Development and structure of the Canadian Forest Fire Weather Index System.
#' 1987. Van Wagner, C.E. Canadian Forestry Service, Headquarters, Ottawa.
#' Forestry Technical Report 35. 35 p.
#'
#' @return A single drought moisture code value
#'
#' @noRd

duff_moisture_code <- function(
    dmc_yda, temp, rh, prec, lat, mon,
    lat.adjust = TRUE) {
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
  # constrain low end of temperature
  temp[temp < (-1.1)] <- -1.1
  # Eq. 16 - The log drying rate
  rk <- 1.894 * (temp + 1.1) * (100 - rh) * ell01[mon] * 1e-04
  # Adjust the day length and thus the drying r, based on latitude and month
  if (lat.adjust) {
    lat.sel <- lat <= 30 & lat > 10
    if(any(lat.sel))rk[lat.sel] <- 1.894 * (temp[lat.sel] + 1.1) * (100 - rh[lat.sel]) * ell02[mon] * 1e-04
    lat.sel <- lat <= -10 & lat > -30
    if(any(lat.sel))rk[lat.sel] <- 1.894 * (temp[lat.sel] + 1.1) * (100 - rh[lat.sel]) * ell03[mon] * 1e-04
    lat.sel <- lat <= -30 & lat >= -90
    if(any(lat.sel))rk[lat.sel] <- 1.894 * (temp[lat.sel] + 1.1) * (100 - rh[lat.sel]) * ell04[mon] * 1e-04
    lat.sel <- lat <= 10 & lat > -10
    if(any(lat.sel))rk[lat.sel] <- 1.894 * (temp[lat.sel] + 1.1) * (100 - rh[lat.sel]) * 9 * 1e-04
  }
  # Constrain P
  # if leq 1.5
  pr <- prec
  prec.sel <- prec <= 1.5
  if(any(prec.sel))pr[prec.sel] <- dmc_yda[prec.sel]
  # else
  if(any(!prec.sel)){
    b <- dmc_yda[!prec.sel]
    dmc_yda.sel <- dmc_yda[!prec.sel]
    b[dmc_yda.sel <= 33] <- 100 / (0.5 + 0.3 * dmc_yda.sel[dmc_yda.sel <= 33])
    b[dmc_yda.sel > 33 & dmc_yda.sel <= 65] <- 14 - 1.3 * log(dmc_yda.sel[dmc_yda.sel > 33 & dmc_yda.sel <= 65])
    b[dmc_yda.sel > 65] <- 6.2 * log(dmc_yda.sel[dmc_yda.sel > 65]) - 17.2
    
    ra <- prec[!prec.sel]
    # Eq. 11 - Net rain amount
    rw <- 0.92 * ra - 1.27
    # Alteration to Eq. 12 to calculate more accurately
    wmi <- 20 + 280 / exp(0.023 * dmc_yda.sel)
    # Eqs. 13a, 13b, 13c
    # Eq. 14 - Moisture content after rain
    wmr <- wmi + 1000 * rw / (48.77 + b * rw)
    # Alteration to Eq. 15 to calculate more accurately
    pr[!prec.sel] <- 43.43 * (5.6348 - log(wmr - 20))
  }
  pr[pr < 0] <- 0
  # Calculate final P (DMC)
  dmc1 <- pr + rk
  dmc1[dmc1 < 0] <- 0
  return(dmc1)
}

.dmcCalc <- function(...) {
  .Deprecated("duff_moisture_code")
  return(duff_moisture_code(...))
}
