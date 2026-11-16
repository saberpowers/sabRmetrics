#' Extract event-level reviews
#'
#' When a reviewed play ends the plate appearance (e.g. an ABS challenge on strike three or ball
#' four), the MLB statsapi attaches the review details to the event (plate appearance) rather than
#' to the play (pitch). This function finds those reviews and identifies the play to which each
#' one belongs: the final pitch of the event (or the final play, if the event has no pitch).
#'
#' An event can have more than one review (the others are listed in `additionalReviews`). Because
#' a play can hold only one review, we keep the ABS challenge if there is one, else the first.
#'
#' @param event_data dataframe of events (`liveData$plays$allPlays`) from the statsapi game feed
#'
#' @return a dataframe with one row per reviewed event, with columns `event_index`, `play_index`,
#'   `event_is_overturned`, `event_review_type`, `event_challenge_team_id` and
#'   `event_challenge_player_id`
#'
extract_event_review <- function(event_data) {

  tidy_review <- function(review) {
    tibble::tibble(
      event_is_overturned = replace_null(review$isOverturned),
      event_review_type = review$reviewType,
      event_challenge_team_id = replace_null(review$challengeTeamId, replacement = NA_integer_),
      event_challenge_player_id = replace_null(review$player$id, replacement = NA_integer_)
    )
  }

  review_data <- event_data$reviewDetails
  is_reviewed <- which(!is.na(replace_null(review_data$reviewType)))

  event_review <- lapply(
    X = is_reviewed,
    FUN = function(i) {
      review <- tidy_review(review_data[i, ])
      if (!is.null(review_data$additionalReviews[i][[1]])) {
        review <- dplyr::bind_rows(review, tidy_review(review_data$additionalReviews[[i]]))
      }
      play_data <- event_data$playEvents[[i]]
      is_pitch <- play_data$type == "pitch"
      review |>
        dplyr::slice(order(event_review_type != "MJ")) |>   # put the ABS challenge first, if any
        dplyr::slice(1) |>
        tibble::add_column(
          event_index = event_data$about$atBatIndex[i],
          play_index = max(play_data$index[is_pitch | !any(is_pitch)]),
          .before = 1
        )
    }
  )

  # Start from an empty dataframe so that the columns exist even if no event was reviewed
  empty <- tibble::tibble(
    event_index = integer(),
    play_index = integer(),
    event_is_overturned = logical(),
    event_review_type = character(),
    event_challenge_team_id = integer(),
    event_challenge_player_id = integer()
  )

  return(dplyr::bind_rows(empty, event_review))
}
