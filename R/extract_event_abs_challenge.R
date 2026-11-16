#' Extract event-level ABS challenges
#'
#' When a challenged pitch ends the plate appearance (strike three or ball four), the MLB statsapi
#' attaches the review details to the event (plate appearance) rather than to the play (pitch).
#' This function finds those ABS challenges and identifies the pitch to which each one belongs:
#' the final pitch of the event.
#'
#' The ABS challenge is usually the event's main review, but it can also be listed under
#' `additionalReviews` (when another review, e.g. of a tag play, holds the main slot).
#'
#' @param event_data dataframe of events (`liveData$plays$allPlays`) from the statsapi game feed
#'
#' @return a dataframe with one row per event with an event-level ABS challenge, with columns
#'   `event_index`, `play_index`, `event_is_overturned`, `event_review_type`,
#'   `event_challenge_team_id` and `event_challenge_player_id`
#'
extract_event_abs_challenge <- function(event_data) {

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

  event_abs_challenge <- lapply(
    X = is_reviewed,
    FUN = function(i) {
      review <- tidy_review(review_data[i, ])
      if (!is.null(review_data$additionalReviews[i][[1]])) {
        review <- dplyr::bind_rows(review, tidy_review(review_data$additionalReviews[[i]]))
      }
      play_data <- event_data$playEvents[[i]]
      review <- dplyr::filter(review, event_review_type == "MJ")
      if (nrow(review) == 0 || !any(play_data$type == "pitch")) {
        return(NULL)
      }
      review |>
        dplyr::slice(1) |>
        dplyr::mutate(
          event_index = event_data$about$atBatIndex[i],
          play_index = max(play_data$index[play_data$type == "pitch"]),
          .before = 1
        )
    }
  )

  # Start from an empty dataframe so that the columns exist even if there is no such challenge
  empty <- tibble::tibble(
    event_index = integer(),
    play_index = integer(),
    event_is_overturned = logical(),
    event_review_type = character(),
    event_challenge_team_id = integer(),
    event_challenge_player_id = integer()
  )

  return(dplyr::bind_rows(empty, event_abs_challenge))
}
