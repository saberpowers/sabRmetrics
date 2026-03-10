# Extract game

Extract event and pitch data from the MLB statsapi.

## Usage

``` r
extract_game(game_id)
```

## Arguments

- game_id:

  an integer primary key specifying the game to extract (character is
  okay)

## Value

a list of two dataframes: `event` and `pitch`
