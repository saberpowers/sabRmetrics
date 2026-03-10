# Get TrackMan metrics for pitch data

Use the quadratic coefficients describing the path of the pitch to
calculate TrackMan metrics

## Usage

``` r
get_trackman_metrics(data)
```

## Arguments

- data:

  dataframe with the following columns: ax, ay, az, bx, by, bz, cx, cy,
  cz

## Value

the input dataframe the following columns added: release_x, release_y,
release_z, plate_x, plate_z, horz_break, vert_break, induced_vert_break,
vx0, vy0, vz0, x0, y0, extension, plate_y, plate_time, gravity,
plate_x_line, plate_z_line, plate_z_gravity, t0

## Examples

``` r
data_statsapi <- download_statsapi(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
pitch_statsapi <- data_statsapi$pitch |>
  get_quadratic_coef(source = "statsapi") |>
  get_trackman_metrics()

data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
#> Submitting initial API requests...
#> Attempting to download 1 payload(s)...
pitch_baseballsavant <- data_baseballsavant |>
  get_quadratic_coef(source = "baseballsavant") |>
  get_trackman_metrics()
```
