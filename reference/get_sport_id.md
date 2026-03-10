# Get Sport ID for a specified level

This function returned a vector of sport IDs corresponding to a vector
of levels.

## Usage

``` r
get_sport_id(level)
```

## Arguments

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

## Value

an integer vector of sport IDs
