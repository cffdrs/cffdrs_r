#' Drought Code Calculator
#' 
#' Drought Code Calculation. All code is based on a C code library that was 
#' written by Canadian Forest Service Employees, which was originally based on 
#' the Fortran code listed in the reference below. 
#' All equations in this code refer to that document. Equations and FORTRAN 
#' program for the Canadian Forest Fire Weather Index System. 1985. Van Wagner, 
#' C.E.; Pickett, T.L. Canadian Forestry Service, Petawawa National Forestry 
#' Institute, Chalk River, Ontario. Forestry Technical Report 33. 18 p.
#' Additional reference on FWI system Development and structure of the Canadian 
#' Forest Fire Weather Index System. 1987. Van Wagner, C.E. Canadian Forestry 
#' Service, Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
#' @references \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf} 
#' Development and structure of the Canadian Forest Fire Weather Index System. 
#' 1987. Van Wagner, C.E. Canadian Forestry Service, Headquarters, Ottawa. 
#' Forestry Technical Report 35. 35 p.
#'
#' @param dc_yda     The Drought Code from previous iteration
#' @param temp       Temperature (centigrade)
#' @param rh         Relative Humidity (%)
#' @param prec       Precipitation(mm)
#' @param lat        Latitude (decimal degrees)
#' @param mon        Month (1-12)
#' @param lat.adjust Latitude adjustment (TRUE, FALSE, default=TRUE)
#' 
#' @return A single drought code value
#' @noRd

.dcCalc <- function(dc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE) {

  #Day length factor for DC Calculations
  #20N: North of 20 degrees N
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  #20S: South of 20 degrees S
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
  #Near the equator, we just use 1.4 for all months.
  #Constrain temperature
  temp <- ifelse(temp < (-2.8), -2.8, temp)
  
  #Eq. 22 - Potential Evapotranspiration
  pe <- (0.36 * (temp + 2.8) + fl01[mon]) / 2
  #Daylength factor adjustment by latitude for Potential Evapotranspiration
  if (lat.adjust) {
    pe <- ifelse(lat <= -20, (0.36 * (temp + 2.8) + fl02[mon]) / 2, pe)
    pe <- ifelse(lat > -20 & lat <= 20, (0.36 * (temp + 2.8) + 1.4)/2, pe)
  }
  # Cap potential evapotranspiration at 0 for negative winter DC values
  pe <- ifelse(pe < 0, 0, pe)
  ra <- prec
  #Eq. 18 - Effective Rainfall
  rw <- 0.83 * ra - 1.27
  #Eq. 19
  smi <- 800 * exp(-1 * dc_yda/400)
  #Alteration to Eq. 21
  dr0 <- dc_yda - 400 * log(1 + 3.937 * rw/smi)
  dr0 <- ifelse(dr0 < 0, 0, dr0)
  #if precip is less than 2.8 then use yesterday's DC
  dr <- ifelse(prec <= 2.8, dc_yda, dr0)
  #Alteration to Eq. 23
  dc1 <- dr + pe
  dc1 <- ifelse(dc1 < 0, 0, dc1)
  return(dc1)
}