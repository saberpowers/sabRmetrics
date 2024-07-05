#' Extract game
#' 
#' Extract event and pitch data from the MLB statsapi.
#' 
#' @param game_id an integer primary key specifying the game to extract (character is okay)
#' 
#' @return a list of two dataframes: `event` and `pitch`
#' 
extract_game <- function(game_id) {

  event_endpoint <- glue::glue("https://statsapi.mlb.com/api/v1.1/game/{game_id}/feed/live")
  event_json <- jsonlite::fromJSON(event_endpoint)

  lineup_endpoint <- glue::glue("https://statsapi.mlb.com/api/v1/game/{game_id}/boxscore")
  lineup_json <- jsonlite::fromJSON(lineup_endpoint)

  
  # Extract event data ----

  event_data <- event_json$liveData$plays$allPlays

  event_base_out_state <- track_base_out_by_event(event_data)

  event_without_fielder_id <- tibble::tibble(
    game_id = game_id,
    event_index = event_data$about$atBatIndex,
    inning = event_data$about$inning,
    half_inning = event_data$about$halfInning,
    batter_id = event_data$matchup$batter$id,
    bat_side = event_data$matchup$batSide$code,
    pitcher_id = event_data$matchup$pitcher$id,
    pitch_hand = event_data$matchup$pitchHand$code,
    event = event_data$result$event,
    is_out = event_data$result$isOut,
    runs_on_event = sapply(event_data$runners,
      FUN = function(x) sum(dplyr::coalesce(x$movement$end, "") == "score")
    )
  ) |>
    dplyr::left_join(event_base_out_state, by = "event_index")


  # Extract play data ----

  play_data <- do.call(dplyr::bind_rows, args = event_data$playEvents)

  play_all <- tibble::tibble(
    play_id = play_data$playId,
    action_play_id = replace_null(play_data$actionPlayId),
    game_id = game_id,
    event_index = rep(event_data$about$atBatIndex, times = sapply(event_data$playEvents, nrow)),
    play_index = play_data$index,
    pitch_number = play_data$pitchNumber,
    type = play_data$type,
    is_substitution = replace_null(play_data$isSubstitution),
    player_id = play_data$player$id,
    position = replace_null(play_data$position$code),
    outs = play_data$count$outs,
    post_balls = play_data$count$balls,
    post_strikes = play_data$count$strikes,
    post_disengagements = replace_null(play_data$details$disengagementNum, replacement = 0),
    description = play_data$details$description,
    event = play_data$details$event,
    from_catcher = replace_null(play_data$details$fromCatcher),
    runner_going = replace_null(play_data$details$runnerGoing),
    is_out = play_data$details$isOut,
    pitch_type = play_data$details$type$code,
    ax = play_data$pitchData$coordinates$aX,
    ay = play_data$pitchData$coordinates$aY,
    az = play_data$pitchData$coordinates$aZ,
    vx0 = play_data$pitchData$coordinates$vX0,
    vy0 = play_data$pitchData$coordinates$vY0,
    vz0 = play_data$pitchData$coordinates$vZ0,
    x0 = play_data$pitchData$coordinates$x0,
    z0 = play_data$pitchData$coordinates$z0,
    extension = replace_null(play_data$pitchData$extension),
    strike_zone_top = play_data$pitchData$strikeZoneTop,
    strike_zone_bottom = play_data$pitchData$strikeZoneBottom,
    launch_speed = play_data$hitData$launchSpeed,
    launch_angle = play_data$hitData$launchAngle,
    hit_coord_x = play_data$hitData$coordinates$coordX,
    hit_coord_y = play_data$hitData$coordinates$coordY,
  ) |>
  # Get pre-pitch count and disengagements
  dplyr::group_by(game_id, event_index) |>
  # We have to track the number of disengagements throughout the end of each plate appearance.
  # For some reason, disengagementNum reverts to NA for the final pitch of each plate appearance.
  tidyr::fill(post_disengagements, .direction = "down") |>
  tidyr::replace_na(list(post_disengagements = 0)) |>
  dplyr::mutate(
    pre_balls = dplyr::coalesce(dplyr::lag(post_balls, 1), 0),
    pre_strikes = dplyr::coalesce(dplyr::lag(post_strikes, 1), 0),
    pre_disengagements = dplyr::coalesce(dplyr::lag(post_disengagements, 1), 0),
  ) |>
  dplyr::ungroup()

  pitch <- play_all |>
    dplyr::filter(type == "pitch") |>
    dplyr::select(play_id, game_id, event_index, play_index, pitch_number,
      outs, balls = pre_balls, strikes = pre_strikes,
      description, pitch_type, ax, ay, az, vx0, vy0, vz0, x0, z0, extension,
      strike_zone_top, strike_zone_bottom, launch_speed, launch_angle, hit_coord_x, hit_coord_y
    )
 

  # Extract fielder credits, fielder lineups and substitutions ----

  fielder_credit <- lapply(
    X = event_data$runners,
    FUN = function(x) {
      do.call(dplyr::bind_rows, args = x$credit)
    }
  )
  first_fielder <- do.call(dplyr::bind_rows, args = fielder_credit) |>
    tibble::add_column(
      event_index = rep(event_data$about$atBatIndex, times = sapply(fielder_credit, nrow)),
      .before = 1
    ) |>
    dplyr::group_by(event_index) |>
    dplyr::slice(1) |>
    # We're doing this weird thing instead of dplyr::select because `position` is itself a dataframe
    # within the dataframe. I don't entirely understand this data structure.
    with(tibble::tibble(event_index, first_fielder = position$code))

  starting_lineup_home <- extract_fielding_lineup(players = lineup_json$teams$home$players) |>
    tibble::add_column(half_inning = "top", .before = 1)
  starting_lineup_away <- extract_fielding_lineup(players = lineup_json$teams$away$players) |>
    tibble::add_column(half_inning = "bottom", .before = 1)
  starting_lineup <- dplyr::bind_rows(starting_lineup_home, starting_lineup_away)

  lineup_by_event <- event_without_fielder_id |>
    dplyr::select(event_index, half_inning) |>
    dplyr::left_join(starting_lineup, by = "half_inning", relationship = "many-to-many") |>
    dplyr::group_by(half_inning) |>
    dplyr::mutate(player_id = ifelse(event_index == min(event_index), player_id, NA)) |>
    dplyr::ungroup()

  substitution <- play_all |>
    dplyr::filter(is_substitution, position %in% 2:10) |>   # keep only players who occupy position
    # Keep only the first substitution for each position in each event
    dplyr::group_by(event_index, position) |>
    dplyr::arrange(play_index) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(event_index, position = as.integer(position), player_id)
  
  lineup_by_event_wide <- lineup_by_event |>
    dplyr::left_join(substitution,
      by = c("event_index", "position"),
      suffix = c("_before", "_after")
    ) |>
    dplyr::mutate(player_id = dplyr::coalesce(player_id_before, player_id_after)) |>
    dplyr::group_by(half_inning, position) |>
    tidyr::fill(player_id, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::transmute(event_index, name = glue::glue("fielder_{position}_id"), player_id) |>
    tidyr::pivot_wider(names_from = name, values_from = player_id)

  event <- event_without_fielder_id |>
    dplyr::left_join(first_fielder, by = "event_index") |>
    dplyr::left_join(lineup_by_event_wide, by = "event_index")
  

  # ----
 
  play_base_out_state <- track_base_out_by_play(event_data)

  # This table includes all pitches, pickoff attempts, stepoffs and automatic balls/strikes
  play <- play_all |>
    dplyr::filter(!is.na(play_id)) |>   # remove non-play "actions" like stolen base attempts
    dplyr::left_join(play_base_out_state, by = "play_id") |>
    tidyr::replace_na(
      list(is_stolen_base = FALSE, is_caught_stealing = FALSE, is_defensive_indiff = FALSE)
    ) |>
    dplyr::select(play_id, game_id, event_index, play_index, pitch_number,
      pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, pre_outs, pre_balls, pre_strikes,
      pre_disengagements,
      runs_on_play,
      post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_outs, post_balls, post_strikes,
      post_disengagements, type, runner_going, from_catcher,
      is_pickoff, is_pickoff_error, is_stolen_base, is_caught_stealing, is_defensive_indiff
    )


  return(
    list(
      event = event,
      pitch = pitch,
      play = play
    )
  )
}
