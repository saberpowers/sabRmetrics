# Sanitize game type

Check that a character vector of game types are all valid game types.

## Usage

``` r
sanitize_game_type(game_type)
```

## Arguments

- game_type:

  character vector of game types. Valid game types are:

  - "R" for Regular Season

  - "F" for First-Round Playoff Series (aka wild card)

  - "D" for Division Series (or minor-league quarter-finals)

  - "L" for League Championship Series (or minor league semi-finals)

  - "W" for World Series (or minor league championship)

  - "A" for All-Star Game

  - "E" for Exhibition

## Value

simply returns the input vector `game_type` but throws error if invalid
types are present
