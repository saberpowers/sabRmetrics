# Get league level by year

The minor leagues have gone through significant restructuring a couple
of times (1990 and 2020), so we can't statically map each league to a
level. This function takes a vector of years and returns a mapping from
league_id to level for each year.

## Usage

``` r
get_league_level_by_year(year)
```

## Arguments

- year:

  integer vector of years

## Value

a table of `level` indexed by `league_id` and `year`

## Examples

``` r
league_level_2025 <- get_league_level_by_year(2025)
league_level_2017 <- get_league_level_by_year(2017)
```
