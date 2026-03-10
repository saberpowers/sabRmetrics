# Track base-out state by event

Post-event base-out state is given to us for free. We get pre-event
base-out state based on the post-event base-out state of the previous
event.

## Usage

``` r
track_base_out_by_event(event_data)
```

## Arguments

- event_data:

  This is the liveData\$plays\$allPlays section of the JSON from a
  game's GUMBO feed

## Value

a tibble with the following columns: event_index, pre_runner_1b_id,
pre_runner_2b_id, pre_runner_3b_id, pre_out, post_runner_1b_id,
post_runner_2b_id, post_runner_3b_id, post_out
