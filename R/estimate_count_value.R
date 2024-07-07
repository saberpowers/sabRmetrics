#' Estimate count value
#' 
#' Compute the average change in base-out run expectancy for all plate appearances that run
#' through each count.
#' 
#' @param play dataframe of play data from \code{\link{download_statsapi}}
#' @param event dataframe of event data from \code{\link{download_statsapi}}
#' @param base_out_run_exp dataframe of base-out run expectancy from
#'   \code{\link{estimate_base_out_run_exp}}
#' 
#' @return a dataframe of `count_value` indexed by `balls` and `strikes`
#' 
#' @export
#' 
estimate_count_value <- function(play, event, base_out_run_exp) {

  data <- play |>
    dplyr::filter(type == "pitch", pre_balls < 4, pre_strikes < 3) |>  # this can happen in the data
    dplyr::select(game_id, event_index, pre_balls, pre_strikes, post_balls, post_strikes,
      dplyr::starts_with("pre_runner_"), pre_outs,
      dplyr::starts_with("post_runner_"), post_outs, runs_on_play
    ) |>
    # Join in pre-event run expectancy
    dplyr::mutate(
      runner_1b = !is.na(pre_runner_1b_id),
      runner_2b = !is.na(pre_runner_2b_id),
      runner_3b = !is.na(pre_runner_3b_id),
      outs = pre_outs
    ) |>
    dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
    dplyr::rename(pre_exp_runs = exp_runs) |>
    # Join in post-event run expectancy
    dplyr::mutate(
      runner_1b = !is.na(post_runner_1b_id) & post_outs < 3,
      runner_2b = !is.na(post_runner_2b_id) & post_outs < 3,
      runner_3b = !is.na(post_runner_3b_id) & post_outs < 3,
      outs = post_outs
    ) |>
    dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
    dplyr::rename(post_exp_runs = exp_runs)

  # Compute average change in run expectancy by count
  nonterminal_value <- data |>
    dplyr::group_by(balls = pre_balls, strikes = pre_strikes) |>
    dplyr::summarize(
      count_value = mean(runs_on_play + post_exp_runs - pre_exp_runs),
      .groups = "drop"
    )

  walk_value <- data |>
    dplyr::filter(post_balls == 4) |>
    # The average change in run expectancy on a walk is slightly different based on strikes,
    # but for our purposes we want to treat each walk the same regardless of strikes
    # (while still maintaining a row for each number of strikes).
    dplyr::group_by(balls = post_balls, strikes = post_strikes) |>
    dplyr::summarize(
      n = dplyr::n(),
      count_value = mean(runs_on_play + post_exp_runs - pre_exp_runs),
      .groups = "drop"
    ) |>
    dplyr::mutate(count_value = weighted.mean(count_value, w = n)) |>
    dplyr::select(balls, strikes, count_value)

  strikeout_value <- data |>
    dplyr::filter(post_strikes == 3) |>
    # The average change in run expectancy on a strikeout is slightly different based on balls,
    # but for our purposes we want to treat each strikeout the same regardless of balls.
    # (while still maintaining a row for each number of balls).
    dplyr::group_by(balls = post_balls, strikes = post_strikes) |>
    dplyr::summarize(
      n = dplyr::n(),
      count_value = mean(runs_on_play + post_exp_runs - pre_exp_runs),
      .groups = "drop"
    ) |>
    dplyr::mutate(count_value = weighted.mean(count_value, w = n)) |>
    dplyr::select(balls, strikes, count_value)

  count_value <- dplyr::bind_rows(
    nonterminal_value,
    walk_value,
    strikeout_value
  )
  
  return(count_value)
}
