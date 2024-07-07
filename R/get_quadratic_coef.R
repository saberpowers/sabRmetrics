#' Get quadratic coefficients for pitch data
#' 
#' Find coefficients such that the path of the ball through time is given by:
#'   x(t) = ax * t^2 / 2 + bx * t + cx
#'   y(t) = ay * t^2 / 2 + by * t + cy
#'   z(t) = az * t^2 / 2 + bz * t + cz
#' We start with velocity and location of ball when y = 50 and use extension to derive coefficients
#' 
#' @param data a dataframe with the columns required for calculating the quadratic coefficients.
#'   If `source = "statsapi", then ax, ay, az, vx0, vy0, vz0, x0, z0, and extension are required.
#'   If `source = "baseballsavant", then ax, ay, az, vx0, vy0, vz0, release_pos_x, release_pos_y,
#'   and release_pos_z are required.
#' @param source character string, either "statsapi" (default) or "baseballsavant"
#' 
#' @return the input dataframe the following columns added: bx, by, bz, cx, cy, cz, y0, t0
#' 
#' @export
#' 
get_quadratic_coef <- function(data, source = c("statsapi", "baseballsavant")) {

  source <- match.arg(source)

  if (source == "statsapi") {

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

  } else if (source == "baseballsavant") {

    quadratic_coef <- data |>
      dplyr::mutate(
        cx = release_pos_x,
        cy = release_pos_y,
        cz = release_pos_z,
        # y0 is the value of y at the time when y = 50 (so it is defintionally 50)
        y0 = 50,
        # t0 is the time corresponding to vx0, vy0, vz0, x0, z0 (i.e. the time when y = 50)
        # Calculate time from y0 to release point, and then negate that time
        t0 = -(-vy0 - sqrt(vy0^2 - 4 * (ay / 2) * (y0 - cy))) / (2 * (ay / 2)),
        # Backtrack velocities by t0 time
        bx = vx0 + (-t0) * ax,
        by = vy0 + (-t0) * ay,
        bz = vz0 + (-t0) * az
      )
  }

  return(quadratic_coef)
}