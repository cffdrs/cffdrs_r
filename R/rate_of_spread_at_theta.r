#' Rate of spread at a point along the perimeter calculator
#'
#' @description Computes the Rate of Spread at any point along the perimeter of
#' an elliptically shaped fire. Equations are from Wotton et. al. (2009).
#'
#' Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to
#' the 1992 Canadian forest fire behavior prediction system. Nat. Resour.
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario,
#' Canada. Information Report GLC-X-10, 45p.
#'
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param ROS      Rate of Spread (m/min)
#' @param FROS     Flank Fire Rate of Spread (m/min)
#' @param BROS     Back Fire Rate of Spread (m/min)
#' @param THETA    Angle to calculate Rate of Spread for
#' @param DEGREES  Whether THETA is is degrees (default = TRUE) - radians if FALSE
#'
#' @returns ROSTHETA - Rate of spread at point theta(m/min)
#'
#' @noRd

rate_of_spread_at_theta <- function(ROS, FROS, BROS, THETA, DEGREES = TRUE) {
  if (DEGREES) {
    THETA <- (THETA %% 360) * pi / 180
  }
  c1 <- cos(THETA)
  s1 <- sin(THETA)
  c1 <- ifelse(c1 == 0, cos(THETA + .001), c1)
  # Eq. 94 - Calculate the Rate of Spread at point THETA
  # large equation, view the paper to see a better representation
  ROStheta <- (
    ((ROS - BROS) / (2 * c1))
    + (((ROS + BROS) / (2 * c1))
    * (
        (FROS * c1 * sqrt(FROS * FROS * c1 * c1 + (ROS * BROS) * s1 * s1)
          - ((ROS * ROS - BROS * BROS) / 4) * s1 * s1)
        / (FROS * FROS * c1 * c1
            + ((ROS + BROS) / 2) * ((ROS + BROS) / 2) * s1 * s1)
      ))
  )
  return(ROStheta)
}

.ROSthetacalc <- function(...) {
  .Deprecated("rate_of_spread_at_theta")
  return(rate_of_spread_at_theta(...))
}
