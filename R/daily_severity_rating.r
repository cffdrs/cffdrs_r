#' Daily Severity Rating Calculator
#'
#' @description Computes the Daily Severity Rating From the FWI System. Equations
#' are from Van Wagner (1985) as listed below, except for the modification for
#' fbp taken from FCFDG (1992).
#'
#' Equations and FORTRAN program for the Canadian Forest Fire
#' Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L.
#' Canadian Forestry Service, Petawawa National Forestry
#' Institute, Chalk River, Ontario. Forestry Technical Report 33.
#' 18 p.
#'
#' Forestry Canada  Fire Danger Group (FCFDG) (1992). Development and
#' Structure of the Canadian Forest Fire Behavior Prediction System."
#' Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario.
#'
#' @param fwi Fire Weather Index
#'
#' @returns DSR - Daily Severity Rating
#'
#' @noRd

daily_severity_rating <- function(fwi) {
    return(0.0272 * (fwi^1.77))
}
