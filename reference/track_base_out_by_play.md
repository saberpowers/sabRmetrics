# Track base-out state by play

This function uses play-by-play runner movement to track the base-out
state (and runs scored) before and after each play (pitch, pickoff
attempt, etc.) in a single game.

## Usage

``` r
track_base_out_by_play(event_data)
```

## Arguments

- event_data:

  This is the liveData\$plays\$allPlays section of the JSON from a
  game's GUMBO feed

## Value

a tibble with the following columns: play_id, pre_runner_1b_id,
pre_runner_2b_id, pre_runner_3b_id, pre_out, post_runner_1b_id,
post_runner_2b_id, post_runner_3b_id, post_out, runs_on_play
is_pickoff_2, is_pickoff_error_2, is_stolen_base_2,
is_caught_stealing_2, is_defensive_indiff_2
