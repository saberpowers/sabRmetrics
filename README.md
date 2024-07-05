# sabRmetrics

The primary purpose of this package is to download high-fidelity MLB data from the statsapi.mlb.com and to organize the resulting data in a collection of tables (as opposed to the single table). This package also has functionality to download data from baseballsavant.mlb.com because some Statcast data are not available through the statsapi. The package also offers code for fundamental modeling such as base-out run expectancy and count values.

## Installation

```R
devtools::install_github(repo = "saberpowers/sabRmetrics")
```

## Core functionality

```R
# Typically takes about 5 seconds for a single game
data_statsapi <- sabRmetrics::download_statsapi(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)

# You can also run this function with parallel computation if you want to download more games
#cluster <- parallel::makeCluster(parallel::detectCores())
#data_statsapi <- sabRmetrics::download_statsapi(
#  start_date = "2024-01-01",
#  end_date = "2024-12-31",
#  cl = cluster
#)
#parallel::stopCluster(cluster)
```

```R
# Typically takes about 30 seconds for the first query of a single game but drastically speeds
# up for subsequent queries of the same game because the API caches the results of the query
data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)

# You can also run this function with parallel computation if you want to download more games
#cluster <- parallel::makeCluster(parallel::detectCores())
#data_statsapi <- sabRmetrics::download_baseballsavant(
#  start_date = "2024-01-01",
#  end_date = "2024-12-31",
#  cl = cluster
#)
#parallel::stopCluster(cluster)
```
