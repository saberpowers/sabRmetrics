# Estimate count value

Compute the average change in base-out run expectancy for all plate
appearances that run through each count.

## Usage

``` r
estimate_count_value(play, event, base_out_run_exp)
```

## Arguments

- play:

  dataframe of play data from
  [`download_statsapi`](https://saberpowers.github.io/sabRmetrics/reference/download_statsapi.md)

- event:

  dataframe of event data from
  [`download_statsapi`](https://saberpowers.github.io/sabRmetrics/reference/download_statsapi.md)

- base_out_run_exp:

  dataframe of base-out run expectancy from
  [`estimate_base_out_run_exp`](https://saberpowers.github.io/sabRmetrics/reference/estimate_base_out_run_exp.md)

## Value

a dataframe of `count_value` indexed by `balls` and `strikes`

## Examples

``` r
data_statsapi <- download_statsapi(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
base_out_run_exp <- estimate_base_out_run_exp(event = data_statsapi$event)
count_value <- estimate_count_value(
  play = data_statsapi$play,
  event = data_statsapi$event,
  base_out_run_exp = base_out_run_exp
)
```
