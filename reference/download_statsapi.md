# Download data from statsapi.mlb.com

Download data from all *regular season* MLB games within a specified
date range.

## Usage

``` r
download_statsapi(
  start_date,
  end_date,
  level = "MLB",
  game_type = "R",
  cl = NULL
)
```

## Arguments

- start_date:

  first date included in the download

- end_date:

  last date included in the download

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

- game_type:

  character vector of game types to include. Options are "R" (regular
  sesason), "F" (first-round playoff series, aka wild card), "D"
  (division series), "L" (league championship series), "W" (world
  series), "S" (spring training), "A" (all-star game), "E" (exhibition).
  Default is "R".

- cl:

  optional cluster object for parallel computation, default is NULL (not
  parallel)

## Value

a list of four dataframes: `event`, `pitch`, `play` and game

## Examples

``` r
# Typically takes about 5 seconds for a single day
data_statsapi <- download_statsapi(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)

# You can also run this function with parallel computation if you want to download more games
if (FALSE) { # \dontrun{
  cluster <- parallel::makeCluster(parallel::detectCores())
  data_statsapi <- download_statsapi(
    start_date = "2024-01-01",
    end_date = "2024-12-31",
    cl = cluster
  )
  parallel::stopCluster(cluster)
} # }
```
