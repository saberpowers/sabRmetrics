# Sanitize level

Check that a character vector of levels are all valid levels.

## Usage

``` r
sanitize_level(level)
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

simply returns the input vector `level` but throws error if invalid
levels are present
