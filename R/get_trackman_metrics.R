#' Get TrackMan metrics for pitch data
#' 
#' Use the quadratic coefficients describing the path of the pitch to calculate TrackMan metrics
#' 
#' @param data dataframe with the following columns: ax, ay, az, bx, by, bz, cx, cy, cz
#' 
#' @return the input dataframe the following columns added:
#'   release_x, release_y, release_z, plate_x, plate_z, horz_break, vert_break, induced_vert_break,
#'   vx0, vy0, vz0, x0, y0, extension,
#'   plate_y, plate_time, gravity, plate_x_line, plate_z_line, plate_z_gravity, t0
#' 
#' @export
#' 
get_trackman_metrics <- function(data) {

  trackman_metrics <- data |>
    dplyr::mutate(

      release_x = cx,
      release_y = cy,
      release_z = cz,

      # Calculate plate location
      plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
      # Solve quadratic equation to get the time at which ball reaches front of plate
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_x = ax * plate_time^2 / 2 + bx * plate_time + cx,
      plate_z = az * plate_time^2 / 2 + bz * plate_time + cz,

      # Set up some intermediate variables for calculating breaks
      gravity = -32.17,   # feet per second per second
      plate_x_line = bx * plate_time + cx,
      plate_z_line = bz * plate_time + cz,
      plate_z_gravity = gravity * plate_time^2 / 2 + bz * plate_time + cz,

      # SP: I'm reconstructing these from memory, so not 100% sure they're correct
      horz_break = 12 * (plate_x - plate_x_line),               # measured in inches
      vert_break = 12 * (plate_z - plate_z_line),               # measured in inches
      induced_vert_break = 12 * (plate_z - plate_z_gravity),    # measured in inches

      # Also recover the TrackMan metrics necessary for calculating the quadratic coefficients
      # t0 is the time when y = 50 (necessary for calculating velocity and x/z location at time t0)
      t0 = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - 50))) / (2 * (ay / 2)),
      vx0 = ax * t0 + bx,
      vy0 = ay * t0 + by,
      vz0 = az * t0 + bz,
      x0 = ax * t0^2 / 2 + bx * t0 + cx,
      z0 = az * t0^2 / 2 + bz * t0 + cz,
      extension = 60.5 - cy,
      release_speed = sqrt(bx^2 + by^2 + bz^2)
    )
  
  return(trackman_metrics)
}
