# Download MLB data from baseballsavant.mlb.com

Loop over five days at a time to download data from the Statcast search
API at Baseball Savant. This includes swing tracking data not available
through the MLB statsapi.

## Usage

``` r
download_baseballsavant(
  start_date,
  end_date,
  game_type = "R",
  cl = NULL,
  verbose = TRUE
)
```

## Arguments

- start_date:

  first date included in the download

- end_date:

  last date included in the download

- game_type:

  character vector of game types to include. Options are "R" (regular
  sesason), "F" (first-round playoff series, aka wild card), "D"
  (division series), "L" (league championship series), "W" (world
  series), "S" (spring training), "A" (all-star game), "E" (exhibition).
  Default is "R".

- cl:

  optional cluster object for parallel computation, default is NULL (not
  parallel)

- verbose:

  logical, should progress be printed to console?

## Value

a dataframe with one row per pitch and all available columns

## Details

The baseballsavant API limits queries to 25,000 rows, so we run several
queries if necessary. If any of the queries returns exactly 25,000 rows,
this indicates that the user probably has not gotten all of the expected
data, so this function throws a warning.

## Examples

``` r
# Typically takes about 30 seconds for the first query of a single day but drastically speeds
# up for subsequent queries of the same game because the API caches the results of the query
data_baseballsavant <- download_baseballsavant(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
#> Submitting initial API requests...
#> Attempting to download 1 payload(s)...

# You can also run this function with parallel computation if you want to download more games
if (FALSE) { # \dontrun{
  cluster <- parallel::makeCluster(parallel::detectCores())
  data_baseballsavant <- download_baseballsavant(
    start_date = "2024-01-01",
    end_date = "2024-12-31",
    cl = cluster
  )
  parallel::stopCluster(cluster)
} # }
```
