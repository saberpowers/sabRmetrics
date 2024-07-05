#' Get quadratic coefficients for pitch data
#' 
#' Find coefficients such that the path of the ball through time is given by:
#'   x(t) = ax * t^2 / 2 + bx * t + cx
#'   y(t) = ay * t^2 / 2 + by * t + cy
#'   z(t) = az * t^2 / 2 + bz * t + cz
#' We start with velocity and location of ball when y = 50 and use extension to derive coefficients
#' 
#' @param data a dataframe with the following columns: ax, ay, az, vx0, vy0, vz0, x0, z0, extension
#' 
#' @return the input dataframe the following columns added: cy, y0, t0, bx, by, bz, cx, cz
#' 
#' @export
#' 
get_quadratic_coef <- function(data) {

  quadratic_coef <- data |>
    dplyr::mutate(
      cy = 60.5 - extension,
      # y0 is the value of y at the time when y = 50 (so it is defintionally 50)
      y0 = 50,
      # t0 is the time corresponding to vx0, vy0, vz0, x0, z0 (i.e. the time when y = 50)
      # Calculate time from y0 to release point, and then negate that time
      t0 = -(-vy0 - sqrt(vy0^2 - 4 * (ay / 2) * (y0 - cy))) / (2 * (ay / 2)),
      # Backtrack velocities by t0 time
      bx = vx0 + (-t0) * ax,
      by = vy0 + (-t0) * ay,
      bz = vz0 + (-t0) * az,
      # Backtrack locations by t0 time
      cx = x0 + (-t0) * vx0 + (-t0)^2 * ax / 2,
      cz = z0 + (-t0) * vz0 + (-t0)^2 * az / 2
    )

  return(quadratic_coef)
}