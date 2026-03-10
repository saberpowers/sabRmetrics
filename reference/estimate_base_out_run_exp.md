# Estimate base-out run expectancy

Model base-out transitions as a Markov chain and find the probability of
each terminal state (including runs scored) from any initial state.

## Usage

``` r
estimate_base_out_run_exp(event)
```

## Arguments

- event:

  dataframe of event data from
  [`download_statsapi`](https://saberpowers.github.io/sabRmetrics/reference/download_statsapi.md)

## Value

a dataframe of `exp_runs` indexed by `runner_1b`, `runner_2b`,
`runner_3b` and `out`

## Examples

``` r
data_statsapi <- download_statsapi(
  start_date = "2024-07-01",
  end_date = "2024-07-01"
)
base_out_run_exp <- estimate_base_out_run_exp(event = data_statsapi$event)
```
