# Download schedule

Download a table of MLB or AAA games within a specified date range (must
be same calendar year).

## Usage

``` r
download_schedule(
  start_date,
  end_date,
  level = c("MLB", "AAA", "AA", "A+", "A"),
  game_type = "R"
)
```

## Arguments

- start_date:

  first date included in the download

- end_date:

  last date included in the download

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

a dataframe of games, with columns `game_id`, `year`, `date`,
`team_id_away`, `team_id_home`, `venue_id`

## Examples

``` r
schedule_mlb_2025 <- download_schedule(
  start_date = "2025-01-01",
  end_date = "2025-12-31",
  level = "MLB"
)
schedule_aaa_2024 <- download_schedule(
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  level = "AAA"
)
```
