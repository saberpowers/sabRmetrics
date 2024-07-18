# sabRmetrics

[<img
src="https://github.com/saberpowers/sabRmetrics/workflows/R-CMD-check/badge.svg"
target="_blank" alt="R build status" />](https://github.com/saberpowers/sabRmetrics/actions)

The highest-fidelity raw MLB data are available from statsapi.mlb.com and baseballsavant.mlb.com, and some minor league data are available from these APIs as well. This package provides functions to query those APIs, returning the data in a collection of well-factored tables (as opposed to returning a single table). Additionally, this package offers functions to estimate base-out run expectancy; estimate count values; and recover the full estimated quadratic trajectory of a pitch from pitch tracking data.

## Installation

```R
devtools::install_github(repo = "saberpowers/sabRmetrics")
```

## Downloading data

Most important data are available from statsapi.mlb.com. The function below returns four tables: game, event, pitch and play.
- The *game* table holds game-level information, such as teams, venue and date.
- The *event* table holds plate-appearance-level information, such as base-out state, players and outcome.
- The *pitch* table holds pitch-level information, such as pitch outcomes, pitch tracking and batted ball tracking.
- The *play* table is similar to the pitch table except that it also includes data regarding pre-/post-base-out-state, including pickoff attempts and stolen base attempts (no tracking data).

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

Some unique Statcast data are not available from statsapi.mlb.com, such as pitch spin and swing tracking. For these data, we turn to baseballsavant.mlb.com. The function below returns pitch-level data that can be joined with the pitch table from statsapi.mlb.com using columns `game_id`, `year`, `event_index` and `pitch_number`.

```R
# Typically takes about 30 seconds for the first query of a single game but drastically speeds
# up for subsequent queries of the same game because the API caches the results of the query
data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)

# You can also run this function with parallel computation if you want to download more games
#cluster <- parallel::makeCluster(parallel::detectCores())
#data_baseballsavant <- sabRmetrics::download_baseballsavant(
#  start_date = "2024-01-01",
#  end_date = "2024-12-31",
#  cl = cluster
#)
#parallel::stopCluster(cluster)
```

## Fitting fundamental models

Possibly the most fundamental model in sabermetrics is base-out run expectancy. The function below fits a Markov model assuming the state is specified by the bases occupied and the number of outs, using the empirical transition probabilities between states.

```R
# You probably want at least one full season of data for this, maybe as many as three seasons
base_out_run_exp <- sabRmetrics::estimate_base_out_run_exp(event = data_statsapi$event)
```

A refinement on base-out run expectancy would be to include the ball-strike count in the state. Rather than expanding the Markov state to include count, the function below calculates the expected change in base-out run expectancy result from a plate appearance, conditional on the plate appearance passing through the specified count. We interpret this as the run value of the count, which allows us to measure the outcomes of individual pitches on the run scale.

```R
# You probably want at least one full season of data for this, maybe as many as three seasons
count_value <- sabRmetrics::estimate_count_value(
  play = data_datsapi$play,
  event = data_stastapi$event,
  base_out_run_exp = base_out_run_exp
)
```

Lastly, if you are working with pitch tracking data, a fundamentally useful step is to recreate the full estimated quadratic trajectory of the pitch. The vast majority of well-known pitch tracking metrics (e.g. horizontal/vertical break, plate location, approach angle) are derived from this estimated quadratic trajectory. The function `get_quadratic_coef` below calculates the coefficients required to reproduce the full trajectory from the data provided by either statsapi.mlb.com or baseballsavant.mlb.com. The function `get_trackman_metrics` below shows how to derive many well-known pitch tracking metrics from the quadratic coefficients.

```R
pitch_statsapi <- data_statsapi$pitch |>
  sabRmetrics::get_quadratic_coef(source = "statsapi") |>
  sabRmetrics::get_trackman_metrics()

pitch_baseballsavant <- data_baseballsavant |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics()
```
