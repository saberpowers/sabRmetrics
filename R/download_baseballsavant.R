#' Download MLB data from baseballsavant.mlb.com
#' 
#' Loop over five days at a time to download data from the Statcast search API at Baseball Savant.
#' This includes swing tracking data not available through the MLB statsapi.
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' @param verbose logical, should progress be printed to console?
#' 
#' @return a dataframe with one row per pitch and all available columns
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'    data_statsapi <- sabRmetrics::download_statsapi(
#'      start_date = "2024-07-01",
#'      end_date = "2024-07-01"
#'    )
#' }
#' 
download_baseballsavant <- function(start_date, end_date, cl = NULL, verbose = TRUE) {

  pbo <- pbapply::pboptions()   # store inital progress bar options so that we can reset them
  if (!verbose) {
    pbapply::pboptions(type = "none")
  }

  # Trim start date and end date to range of actual games
  schedule <- extract_schedule(start_date, end_date, level = "mlb")
  start_date <- min(schedule$date)
  end_date <- max(schedule$date)

  base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details"

  # Split the dates into 5-day chunks. The Savant API will return at most 25,000 rows. Assuming 300
  # pitches per game, a day with 15 games will have 4,500 pitches. We can safely download 5 days of
  # data, but more days would risk hitting the 25,000-row limit.
  days <- as.numeric(as.Date(end_date) - as.Date(start_date))
  start_date_seq <- as.Date(start_date) + seq(from = 0, by = 5, to = days)
  base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details"
  url_seq <- glue::glue("{base_url}&game_date_gt={start_date_seq}&game_date_lt={start_date_seq + 4}")

  # The baseballsavant API can take some time to respond (often over 1 minute). To speed up this
  # process, we submit all requests right away without waiting for a response. The API gets to work
  # on these queries and returns them the next time we ask for them (later on in the script).
  if (verbose) {
    message("Submitting initial API requests...")
  }
  initial_request <- pbapply::pblapply(
    X = url_seq,
    FUN = function(url) {
      try(httr::GET(url, httr::timeout(1)), silent = TRUE)
    },
    cl = cl
  )

  if (verbose) {
    message(glue::glue("Trying to download {length(url_seq)} payload(s)..."))
  }

  # Initialize our list of data payloads, treating all of them as errors before successful download
  data_list <- lapply(X = url_seq, FUN = function(x) return("error"))
  names(data_list) <- url_seq
  is_error <- sapply(data_list, FUN = function(x) identical(x, "error"))

  while(any(is_error)) {

    data_list[url_seq[is_error]] <- pbapply::pblapply(
      X = url_seq[is_error],
      FUN = function(url) {
        data <- try(
          httr::content(
            httr::GET(url),
            type = "text/csv",
            encoding = "UTF-8",
            name_repair = "universal_quiet",
            show_col_types = FALSE
          )
        )
        if ("try-error" %in% class(data)) {
          return("error")
        } else if (nrow(data) == 25000) {
          warning(
            glue::glue("Exactly 25,000 rows returned for {url}")
          )
        } else if (nrow(data) == 0) { # this can happen when the date range includes no tracked games
          return(NULL)
        }
        return(data)
      },
      cl = cl
    )

    # We only want to re-try to download the failed payloads
    is_error <- sapply(data_list, FUN = function(x) identical(x, "error"))

    if (verbose && sum(is_error) > 0) {
      message(glue::glue("{sum(is_error)} download(s) timed out. Retrying..."))
    }
  }

  pbapply::pboptions(pbo)   # put progress bar options back where we found them

  data <- do.call(dplyr::bind_rows, args = data_list) |>
    tibble::as_tibble() |>
    # re-define columns as needed to match statsapi
    dplyr::mutate(
      event_index = at_bat_number - 1,
      half_inning = dplyr::case_when(
        inning_topbot == "Top" ~ "top",
        inning_topbot == "Bot" ~ "bottom"
      )
    ) |>
    # re-order and re-name columns (but don't drop any)
    dplyr::select(
      # pitch identifiers (for joining on pitch table from statsapi)
      game_id = game_pk,
      year = game_year,
      event_index,
      pitch_number,
      # player and team identifiers
      home_team,
      away_team,
      batter_id = batter,
      bat_side = stand,
      batter_name = player_name,
      pitcher_id = pitcher...8,       # unfortunate consequence of read_csv's name_repair
      pitch_hand = p_throws,
      pre_runner_1b_id = on_1b,
      pre_runner_2b_id = on_2b,
      pre_runner_3b_id = on_3b,
      fielder_2_id = fielder_2...42,  # unfortunate consequence of read_csv's name_repair
      fielder_3_id = fielder_3,
      fielder_4_id = fielder_4,
      fielder_5_id = fielder_5,
      fielder_6_id = fielder_6,
      fielder_7_id = fielder_7,
      fielder_8_id = fielder_8,
      fielder_9_id = fielder_9,
      # context
      inning,
      half_inning,
      outs = outs_when_up,
      balls,
      strikes,
      if_fielding_alignment,
      of_fielding_alignment,
      # pitch tracking
      pitch_type,
      ## here are the features you need to recreate the full quadratic trajectory of the pitch
      ax,
      ay,
      az,
      vx0,
      vy0,
      vz0,
      release_pos_x,
      release_pos_y,
      release_pos_z,
      ## here are the highly interpretable features that are functions of the quadratic trajectory
      release_speed,
      extension = release_extension,
      effective_speed,
      pfx_x,
      pfx_z,
      plate_x,
      plate_z,
      zone,
      ## here are the additional features that can't be derived from the quadratic trajectory
      release_spin_rate,
      spin_axis,
      strike_zone_top = sz_top,
      strike_zone_bottom = sz_bot,
      # swing tracking
      bat_speed,
      swing_length,
      # batted ball tracking
      launch_speed,
      launch_angle,
      expected_woba = estimated_woba_using_speedangle,
      expected_babip = estimated_ba_using_speedangle,
      hit_coord_x = hc_x,
      hit_coord_y = hc_y,
      hit_distance_sc,
      bb_type,
      hit_location,
      # outcome
      type,
      description,
      events,
      woba_denom,
      woba_value,
      babip_value,
      iso_value,
      delta_run_exp,
      delta_home_win_exp,
      # retain all other columns, but keep them last because they don't have a clear use case
      dplyr::everything()
    )

  return(data)
}
