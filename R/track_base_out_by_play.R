#' Track base-out state by play
#' 
#' This function uses play-by-play runner movement to track the base-out state (and runs scored)
#' before and after each play (pitch, pickoff attempt, etc.) in a single game.
#' 
#' @param event_data This is the liveData$plays$allPlays section of the JSON from a game's GUMBO feed
#' 
#' @return a tibble with the following columns:
#'  play_id,
#'  pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, pre_out,
#'  post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_out, runs_on_play
#'  is_pickoff_2, is_pickoff_error_2, is_stolen_base_2, is_caught_stealing_2, is_defensive_indiff_2
#' 
track_base_out_by_play <- function(event_data) {

  # Step 0. Extract runner data ----

  runner_detail_list <- lapply(event_data$runners, function(x) x$detail)
  runner_movement_list <- lapply(event_data$runners, function(x) x$movement)
  runner_length <- sapply(runner_detail_list, function(x) if(is.null(x)) 0 else nrow(x))
  runner_movement <- dplyr::bind_cols(
    do.call(dplyr::bind_rows, args = runner_detail_list),
    do.call(dplyr::bind_rows, args = runner_movement_list)
  ) |>
    tibble::add_column(event_index = rep(event_data$about$atBatIndex, times = runner_length)) |>
    # Limit ourselves to one movement per event/play/runner
    dplyr::group_by(event_index, play_index = playIndex, runner$id) |>
    # Take the last movement to get the end base (start base is provided by `originBase`)
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      event_index,
      play_index,
      event_runner = event,  # sometimes the event (e.g. stolen base) is in runners data
      runner_id = runner$id,
      start_base = dplyr::coalesce(originBase, "batter"),
      end_base = dplyr::coalesce(end, "out"),
      is_out = isOut,
      is_scoring_event = isScoringEvent
    )

  
  # Step 1. Consolidate runner movement within play ID ----

  # We have to do this because, for example, generally a stolen base attempt on a pitch will have
  # two separate rows in the play data: one for the pitch and one for the stolen base attempt.
  # We want to conlidate these into one row of data.

  play_data <- do.call(dplyr::bind_rows, args = event_data$playEvents)

  play_keys <- tibble::tibble(
    play_id = play_data$playId,
    action_play_id = replace_null(play_data$actionPlayId),
    event_index = rep(event_data$about$atBatIndex, times = sapply(event_data$playEvents, nrow)),
    play_index = play_data$index,
    event_play = play_data$details$event  # sometimes the event (e.g. stolen base) is in play data
  )

  runner_movement_consolidated <- play_keys |>
    dplyr::transmute(
      play_id = dplyr::coalesce(play_id, action_play_id),
      event_index,
      play_index,
      event_play
    ) |>
    dplyr::inner_join(runner_movement, by = c("event_index", "play_index")) |>
    dplyr::group_by(play_id, runner_id) |>
    dplyr::arrange(event_index, play_index) |>
    dplyr::summarize(
      start_base = start_base[1],
      end_base = end_base[dplyr::n()],
      outs = sum(end_base == "out"),
      runs = sum(end_base == "score"),
      # Check both `event_play` and `event_runners` for stolen base, etc.
      # Usually it's in both, but sometimes only one or the other will have it.
      is_pickoff = any(
        grepl(
          "Pickoff", c(event_play, event_runner)) &
          !grepl("Pickoff Error", c(event_play, event_runner)
        )
      ),
      is_pickoff_error = any(grepl("Pickoff Error", c(event_play, event_runner))),
      is_stolen_base = any(grepl("Stolen Base", c(event_play, event_runner))),
      is_caught_stealing = any(grepl("Caught Stealing", c(event_play, event_runner))),
      is_defensive_indiff = any(grepl("Defensive Indiff", c(event_play, event_runner))),
      .groups = "drop"
    )
  
 
  # Step 2. Initiate play base-out state from pre-event base-out state ----

  event_base_out_state <- track_base_out_by_event(event_data)

  pre_state <- event_base_out_state |>
    dplyr::select(
      event_index, dplyr::starts_with("pre_"), dplyr::starts_with("post_")
    ) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "pre_", "pre_event_"),
      .cols = dplyr::starts_with("pre_")
    ) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "post_", "post_event_"),
      .cols = dplyr::starts_with("post_")
    ) |>
    dplyr::mutate(
      pre_runner_1b_id = pre_event_runner_1b_id,
      pre_runner_2b_id = pre_event_runner_2b_id,
      pre_runner_3b_id = pre_event_runner_3b_id,
      pre_outs = pre_event_outs
    )

  base_out_state <- play_keys |>
    dplyr::filter(!is.na(play_id)) |>   # remove non-play "actions" like stolen base attempts
    dplyr::select(play_id, event_index, play_index) |>
    dplyr::left_join(pre_state, by = "event_index") |>
    # Initialize post-play outs to match pre-play outs (before accounting for runner movement)
    dplyr::mutate(
      post_runner_1b_id = pre_runner_1b_id,
      post_runner_2b_id = pre_runner_2b_id,
      post_runner_3b_id = pre_runner_3b_id,
      post_outs = pre_outs
    )

 
  # Step 3. Set up and join helper tables with runner movement ----

  runner_movement_from <- list()
  runner_movement_to <- list()

  for (base in c("1B", "2B", "3B")) {

    runner_id_from_string <- glue::glue("runner_id_from_{tolower(base)}")
    runner_id_to_string <- glue::glue("runner_id_to_{tolower(base)}")

    runner_movement_from[[base]] <- runner_movement_consolidated |>
      dplyr::filter(start_base == base) |>
      dplyr::mutate(!!runner_id_from_string := runner_id) |>
      dplyr::select(play_id, dplyr::all_of(runner_id_from_string))

    runner_movement_to[[base]] <- runner_movement_consolidated |>
      dplyr::filter(end_base == base) |>
      dplyr::mutate(!!runner_id_to_string := runner_id) |>
      dplyr::select(play_id, dplyr::all_of(runner_id_to_string))

    base_out_state <- base_out_state |>
      dplyr::left_join(runner_movement_from[[base]], by = "play_id") |>
      dplyr::left_join(runner_movement_to[[base]], by = "play_id")
  }

  play_summary <- runner_movement_consolidated |>
    dplyr::group_by(play_id) |>
    dplyr::summarize(
      outs = sum(outs),
      runs = sum(runs),
      is_pickoff = any(is_pickoff),
      is_pickoff_error = any(is_pickoff_error),
      is_stolen_base = any(is_stolen_base),
      is_caught_stealing = any(is_caught_stealing),
      is_defensive_indiff = any(is_defensive_indiff),
      .groups = "drop"
    )

  base_out_state <- base_out_state |>
    dplyr::left_join(play_summary, by = "play_id") |>
    dplyr::mutate(
      outs = dplyr::coalesce(outs, 0),
      runs = dplyr::coalesce(runs, 0),
      is_pickoff = dplyr::coalesce(is_pickoff, FALSE),
      is_pickoff_error = dplyr::coalesce(is_pickoff_error, FALSE),
      is_stolen_base = dplyr::coalesce(is_stolen_base, FALSE),
      is_caught_stealing = dplyr::coalesce(is_caught_stealing, FALSE),
      is_defensive_indiff = dplyr::coalesce(is_defensive_indiff, FALSE)
    )

  
  # Step 4. Update play base-out states using runner movement ----

  # Iteratively update base-out state play-by-play until the updates propagate through all events

  max_plays_per_event <- base_out_state |>
    dplyr::count(event_index) |>
    with(max(n))

  for (i in 1:max_plays_per_event) {

    # Step 1: Update post state based on runner movement
    base_out_state <- base_out_state |>
      dplyr::mutate(
        post_runner_1b_id = dplyr::case_when(
          !is.na(runner_id_to_1b) ~ runner_id_to_1b,
          !is.na(runner_id_from_1b) ~ NA,
          TRUE ~ pre_runner_1b_id
        ),
        post_runner_2b_id = dplyr::case_when(
          !is.na(runner_id_to_2b) ~ runner_id_to_2b,
          !is.na(runner_id_from_2b) ~ NA,
          TRUE ~ pre_runner_2b_id
        ),
        post_runner_3b_id = dplyr::case_when(
          !is.na(runner_id_to_3b) ~ runner_id_to_3b,
          !is.na(runner_id_from_3b) ~ NA,
          TRUE ~ pre_runner_3b_id
        ),
        post_outs = pre_outs + dplyr::coalesce(outs, 0)
      )

    # Step 2: Update pre state based on post state of prior play
    base_out_state <- base_out_state |>
      dplyr::group_by(event_index) |>
      dplyr::mutate(
        pre_runner_1b_id = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_runner_1b_id,
          no = dplyr::lag(post_runner_1b_id, 1)
        ),
        pre_runner_2b_id = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_runner_2b_id,
          no = dplyr::lag(post_runner_2b_id, 1)
        ),
        pre_runner_3b_id = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_runner_3b_id,
          no = dplyr::lag(post_runner_3b_id, 1)
        ),
        pre_outs = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_outs,
          no = dplyr::lag(post_outs, 1)
        )
      ) |>
      dplyr::ungroup()
  }

  base_out_state <- base_out_state |>
    dplyr::select(play_id,
      pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, pre_outs,
      post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_outs, runs_on_play = runs,
      is_pickoff, is_pickoff_error, is_stolen_base, is_caught_stealing, is_defensive_indiff
    )

  return(base_out_state)
}
