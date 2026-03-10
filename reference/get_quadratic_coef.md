# Get quadratic coefficients for pitch data

Find coefficients such that the path of the ball through time is given
by: x(t) = ax \* t^2 / 2 + bx \* t + cx y(t) = ay \* t^2 / 2 + by \* t +
cy z(t) = az \* t^2 / 2 + bz \* t + cz We start with velocity and
location of ball when y = 50 and use extension to derive coefficients

## Usage

``` r
get_quadratic_coef(data, source = c("statsapi", "baseballsavant"))
```

## Arguments

- data:

  a dataframe with the columns required for calculating the quadratic
  coefficients. If `source = "statsapi"`, then `ax`, `ay`, `az`, `vx0`,
  `vy0`, `vz0`, `x0`, `z0`, and `extension` are required. If
  `source = "baseballsavant"`, then `ax`, `ay`, `az`, `vx0`, `vy0`,
  `vz0`, `release_pos_x`, `release_pos_y`, and `release_pos_z` are
  required.

- source:

  character string, either "statsapi" (default) or "baseballsavant"

## Value

the input dataframe the following columns added: bx, by, bz, cx, cy, cz,
y0, t0

## Examples

``` r
data_statsapi <- download_statsapi(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
pitch_statsapi <- data_statsapi$pitch |>
  get_quadratic_coef(source = "statsapi")

data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
#> Submitting initial API requests...
#> Attempting to download 1 payload(s)...
pitch_baseballsavant <- data_baseballsavant |>
  get_quadratic_coef(source = "baseballsavant")
```
