#' Compute base-out run expectancy
#' 
#' Model base-out transitions as a Markov chain and find the probability of each terminal state
#' (including runs scored) from any initial state.
#' 
#' @param event dataframe of event data from \code{\link{download_statsapi}}
#' 
#' @return a dataframe of `exp_runs` indexed by `runner_1b`, `runner_2b`, `runner_3b` and `out`
#' 
#' @export
#' 
compute_base_out_run_exp <- function(event) {
  
  base_out_transition <- event |>
    dplyr::filter(event != "Game Advisory") |>
    dplyr::mutate(
      pre_base_out_state = paste0(
        1 * !is.na(pre_runner_1b_id),
        1 * !is.na(pre_runner_2b_id),
        1 * !is.na(pre_runner_3b_id),
        pre_outs
      ),
      post_base_out_state = paste0(
        1 * !is.na(post_runner_1b_id),
        1 * !is.na(post_runner_2b_id),
        1 * !is.na(post_runner_3b_id),
        post_outs
      ),
      runs = runs_on_event
    ) |>
    # Count the number of each base-out transition
    dplyr::count(pre_base_out_state, post_base_out_state, runs) |>
    # Compute the probability of each base-out transition
    dplyr::group_by(pre_base_out_state) |>
    dplyr::mutate(pre_runs = 0, post_runs = runs, prob = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::bind_rows(
      tibble::tibble(
        pre_base_out_state = c("0000", "0003"),
        pre_runs = c(0, 0),
        post_base_out_state = c("0000", "0003"),
        post_runs = c(0, 0),
        prob = c(0, 1)
      )
    ) |>
    dplyr::select(dplyr::starts_with("pre_"), dplyr::starts_with("post_"), prob)

  # Add an additional dimension (runs already scored) to the state of an inning so that our
  # Markov chain can track the cumulative runs scored in the terminal states.
  base_out_transition_augmented <- tibble::tibble()
  for (extra_runs in 0:9) {   # cap runs already scored at 9 because we have to cap it somewhere
    base_out_transition_augmented <- base_out_transition_augmented |>
      dplyr::bind_rows(
        base_out_transition |>
          dplyr::mutate(
            pre_runs = pre_runs + extra_runs,
            post_runs = pmin(post_runs + extra_runs, 9),
            pre_state = glue::glue("{pre_base_out_state}{pre_runs}"),
            post_state = glue::glue("{post_base_out_state}{post_runs}")
          ) |>
          dplyr::select(pre_state, post_state, prob)
      )
  }
  
  # Convert tibble to matrix for multiplication
  transition_matrix <- base_out_transition_augmented |>
    dplyr::group_by(pre_state, post_state) |>
    dplyr::summarize(prob = sum(prob), .groups = "drop") |>
    dplyr::arrange(post_state) |>
    tidyr::pivot_wider(names_from = post_state, values_from = prob, values_fill = 0) |>
    dplyr::arrange(pre_state) |>
    dplyr::select(-pre_state) |>
    as.matrix()
  
  # Raise transition matrix to 20th power to get terminal state probabilties from each start state.
  # We substitute 20 for infinity because the probability of 20 PA in an inning is very very low.
  transition_matrix_20_steps <- transition_matrix
  for (step in 1:19) {
    transition_matrix_20_steps <- transition_matrix_20_steps %*% transition_matrix
  }
  
  # Extract run expectancy for each start state from the terminal state probabilties
  base_out_run_exp <- transition_matrix_20_steps |>
    tibble::as_tibble() |>
    tibble::add_column(
      pre_state = sort(unique(base_out_transition_augmented$pre_state)), .before = 1
    ) |>
    tidyr::pivot_longer(cols = -pre_state, names_to = "post_state", values_to = "prob") |>
    dplyr::mutate(
      outs = as.integer(substring(post_state, 4, 4)),
      runs = as.integer(substring(post_state, 5, 5))
    ) |>
    dplyr::group_by(pre_state) |>
    dplyr::summarize(exp_runs = weighted.mean(runs, w = prob), .groups = "drop") |>
    dplyr::filter(substring(pre_state, 5, 5) == "0") |>
    dplyr::transmute(
      runner_1b = substring(pre_state, 1, 1) == 1,
      runner_2b = substring(pre_state, 2, 2) == 1,
      runner_3b = substring(pre_state, 3, 3) == 1,
      outs = as.integer(substring(pre_state, 4, 4)),
      exp_runs
    )

  return(base_out_run_exp)
}
