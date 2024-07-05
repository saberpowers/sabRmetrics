#' Track base-out state by event
#' 
#' Post-event base-out state is given to us for free. We get pre-event base-out state based on the
#' post-event base-out state of the previous event.
#'
#' @param event_data This is the liveData$plays$allPlays section of the JSON from a game's GUMBO feed
#' 
#' @return a tibble with the following columns:
#'  event_index,
#'  pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, pre_out,
#'  post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_out
#' 
track_base_out_by_event <- function(event_data) {

  post_state <- tibble::tibble(
    event_index = event_data$about$atBatIndex,
    # We coalesce with NA to handle the case where the value is NULL (e.g. on one reached 3B)
    post_runner_1b_id = dplyr::coalesce(event_data$matchup$postOnFirst$id, NA),
    post_runner_2b_id = dplyr::coalesce(event_data$matchup$postOnSecond$id, NA),
    post_runner_3b_id = dplyr::coalesce(event_data$matchup$postOnThird$id, NA),
    post_outs = event_data$count$outs
  )

  pre_state <- post_state |>
    dplyr::transmute(
      event_index,
      pre_runner_1b_id = dplyr::lag(post_runner_1b_id, 1),
      pre_runner_2b_id = dplyr::lag(post_runner_2b_id, 1),
      pre_runner_3b_id = dplyr::lag(post_runner_3b_id, 1),
      pre_outs = dplyr::lag(post_outs, 1, default = 0) %% 3
    )

  zombie_runner <- dplyr::coalesce(
    sapply(event_data$playEvents, function(x) any(x$details$event == "Runner Placed On Base")),
    FALSE
  )

  zombie_runner_id <- do.call(dplyr::bind_rows, args = event_data$playEvents) |>
    dplyr::filter(details$event == "Runner Placed On Base") |>
    with(player$id)

  pre_state$pre_runner_2b_id[zombie_runner] <- zombie_runner_id

  base_out_state <- pre_state |>
    dplyr::left_join(post_state, by = "event_index") |>
    dplyr::select(
      event_index,
      dplyr::starts_with("pre_"),
      dplyr::starts_with("post_")
    )

  return(base_out_state)
}
