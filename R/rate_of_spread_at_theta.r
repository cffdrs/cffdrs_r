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

# want this to work on columns but ifelse() results in only one warning instead of one per error
rate_of_spread_at_theta <- function(ROS, FROS, BROS, THETA, DEGREES = TRUE) {
  # HACK: rep(NULL, n) returns NULL
  ros_theta <- rep(list(NULL), length(ROS))
  # none of this makes sense if ROS isn't the maximum value and located at (0 == THETA)
  # spread is the same in every direction (since this is a circle) or there is no spread
  circle <- (ROS == FROS & ROS == BROS)
  ros_theta[circle] <- ROS[circle]
  invalid <- !((ROS >= FROS) & (ROS >= BROS)) & !circle
  # these omit previously invalid results so the checks cascade
  if (0 < sum(invalid)) {
    warning(sprintf("Expected ROS >= max(FROS, BROS) but got %f < max(%f, %f))", ROS[invalid], FROS[invalid], BROS[invalid]))
  }
  invalid_bros <- (0 > BROS[!invalid & !circle])
  if (0 < sum(invalid_bros)) {
    warning(sprintf("Expected BROS >= 0 but got %f\n", BROS[!invalid & !circle][invalid_bros]))
  }
  invalid[!invalid & !circle] <- invalid_bros
  invalid_fros <- (0 >= FROS[!invalid & !circle])
  if (0 < sum(invalid_fros)) {
    warning(sprintf("Expected FROS > 0 but got %f\n", FROS[!invalid & !circle][invalid_fros]))
  }
  invalid[!invalid & !circle] <- invalid_fros
  ros_theta[invalid] <- NA
  if (DEGREES) {
    THETA <- (THETA %% 360) * pi / 180
  }
  c1 <- cos(THETA)
  s1 <- sin(THETA)
  # compare within tolerance because (c1 == 0) is unlikely with rounding error
  invalid_cos <- abs(c1) < 0.0001
  c1[invalid_cos] <- cos(THETA[invalid_cos] + 0.0001)
  valid <- sapply(ros_theta, is.null)
  # should be only things that are valid and not circles left to do
  stopifnot(all(valid == (!invalid & !circle)))
  ros <- ROS[valid]
  bros <- BROS[valid]
  fros <- FROS[valid]
  c1 <- c1[valid]
  s1 <- s1[valid]
  # Eq. 94 - Calculate the Rate of Spread at point THETA
  # large equation, view the paper to see a better representation
  ros_theta[valid] <- (
      ((ros - bros) / (2 * c1))
    +
      (
        ((ros + bros) / (2 * c1))
      *
        (
          (
            (
              (fros * c1)
            *
              sqrt(
                (fros * fros * c1 * c1)
              +
                (ros * bros * s1 * s1)
              )
            )
          -
            (
              (
                (
                  (ros * ros)
                -
                  (bros * bros)
                )
              /
                4
              )
            *
              (s1 * s1)
            )
          )
        /
          (
              (fros * fros * c1 * c1)
            +
              (((ros + bros) / 2) * ((ros + bros) / 2) * s1 * s1)
          )
        )
      )
  )
  return(unlist(ros_theta))
}


.ROSthetacalc <- function(...) {
  .Deprecated("rate_of_spread_at_theta")
  return(rate_of_spread_at_theta(...))
}
