# Download player stats season summary

This function this the
https://bdfed.stitch.mlbinfra.com/bdfed/stats/player api to download
player season summary stats. Coverage goes back to 1901 for MLB, 1990
for domestic minor leagues and 2016 for the Dominican Summer League.
Year and level can be provided as vector arugments.

## Usage

``` r
download_season_summary(
  year,
  level = "MLB",
  position = c("hitting", "pitching"),
  game_type = c("R", "P"),
  cl = NULL
)
```

## Arguments

- year:

  integer vector of years

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

- position:

  "hitting" or "pitching" (cannot be both)

- game_type:

  "R" for regular season or "P" for playoffs (cannot be both)

- cl:

  optional cluster object for parallel computation, default is NULL (not
  parallel)

## Value

a table of season summary statistics indexed by year, level, league_id
and player_id

## Examples

``` r
mlb_hitting_2025 <- download_season_summary(year = 2025, level = "MLB", position = "hitting")
aaa_pitching_2025 <- download_season_summary(year = 2025, level = "AAA", position = "pitching")
```
