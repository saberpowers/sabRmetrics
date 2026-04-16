# Download MLB 40-Man Rosters

Retrieves the 40-man roster for all Major League Baseball teams for a
specified date using the MLB Stats API.

## Usage

``` r
download_40man_roster(date = Sys.Date(), cl = NULL)
```

## Arguments

- date:

  A character string representing the date in `YYYY-MM-DD` format.
  Defaults to the current system date.

- cl:

  optional cluster object for parallel computation, default is NULL (not
  parallel)

## Value

A `tibble` containing the following columns:

- **team_id**: The unique numeric identifier for the MLB team.

- **player_id**: The unique numeric identifier for the player.

- **status**: The character code representing the player's current
  roster status. Possible values include:

  - A: Active

  - BRV: Bereavement List

  - D#: Injured \#-Day

  - PL: Paternity List

  - RM: Reassigned to Minors

  - SU: Suspended
