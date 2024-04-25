#' Build Up Index Calculator
#'
#' @description Buildup Index Calculation. All code is based on a C code
#' library that was written by Canadian Forest Service Employees, which was
#' originally based on  the Fortran code listed in the reference below. All
#' equations in this code refer to that document.
#' Equations and FORTRAN program for the Canadian Forest Fire Weather Index
#' System. 1985. Van Wagner, C.E.; Pickett, T.L. Canadian Forestry Service,
#' Petawawa National Forestry Institute, Chalk River, Ontario. Forestry
#' Technical Report 33. 18 p.
#'
#' @param dc Drought Code
#' @param dmc Duff Moisture Code
#'
#' @return A single Build Up Index value
#'
#' @references \url{https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf}
#' Development and structure of the Canadian Forest Fire Weather Index System.
#' 1987. Van Wagner, C.E. Canadian Forestry Service, Headquarters, Ottawa.
#' Forestry Technical Report 35. 35 p.
#' @noRd

buildup_index <- function(dmc, dc) {
  # Eq. 27a
  bui1 <- 0.8 * dc * dmc / (dmc + 0.4 * dc)
  bui1[dmc == 0 & dc == 0] <- 0
  # Eq. 27b - next 3 lines
  p <- rep(0, length(dmc))
  p[dmc != 0] <- (dmc[dmc != 0] - bui1[dmc != 0]) / dmc[dmc != 0]
  cc <- 0.92 + ((0.0114 * dmc)^1.7)
  bui0 <- dmc - cc * p
  # Constraints
  bui0[bui0 < 0] <- 0
  bui1[bui1 < dmc] <- bui0[bui1 < dmc]
  bui1[bui1 >= dmc] <- bui1[bui1 >= dmc]
  return(bui1)
}

.buiCalc <- function(...) {
  .Deprecated("buildup_index")
  return(buildup_index(...))
}
