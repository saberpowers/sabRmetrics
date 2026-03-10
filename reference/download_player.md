# Download player table

Download player information for a vector of years and a vector of
levels.

## Usage

``` r
download_player(
  year = lubridate::year(Sys.Date()),
  level = c("MLB", "AAA", "AA", "A+", "A", "CL", "DSL"),
  cl = NULL
)
```

## Arguments

- year:

  vector of integers (first valid year is 1876)

- level:

  character vector of levels. Valid levels are:

  - "MLB" for Major League Baseball

  - "AAA" for Triple-A

  - "AA" for Double-A

  - "A+" for High-A

  - "A" for Single-A

  - "SS" (defunct since 2020) for Short-Season A and Rookie Advanced

  - "CL" for Complex Leagues

  - "DSL" for Dominican Summer League

- cl:

  optional cluster object for parallel computation, default is NULL (not
  parallel)

## Value

a table of player data, indexed by player_id

## Examples

``` r
player_mlb_2025 <- download_player(year = 2025, level = "MLB")
player_aaa_2024 <- download_player(year = 2024, level = "AAA")
```
