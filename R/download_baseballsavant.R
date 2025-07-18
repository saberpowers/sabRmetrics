#' Download MLB data from baseballsavant.mlb.com
#' 
#' Loop over five days at a time to download data from the Statcast search API at Baseball Savant.
#' This includes swing tracking data not available through the MLB statsapi.
#' 
#' @details The baseballsavant API limits queries to 25,000 rows, so we run several queries if
#'   necessary. If any of the queries returns exactly 25,000 rows, this indicates that the user
#'   probably has not gotten all of the expected data, so this function throws a warning.
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @param game_type character vector of game types to include. Options are "R" (regular sesason),
#'   "F" (first-round playoff series, aka wild card), "D" (division series), "L" (league
#'   championship series), "W" (world series), "S" (spring training), "A" (all-star game),
#'   "E" (exhibition). Default is "R".
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' @param verbose logical, should progress be printed to console?
#' 
#' @return a dataframe with one row per pitch and all available columns
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'    data_baseballsavant <- download_baseballsavant(
#'      start_date = "2024-07-01",
#'      end_date = "2024-07-01"
#'    )
#' }
#' 
download_baseballsavant <- function(start_date,
                                    end_date,
                                    game_type = "R",
                                    cl = NULL,
                                    verbose = TRUE) {

  game_type <- sanitize_game_type(game_type)

  pbo <- pbapply::pboptions()   # store inital progress bar options so that we can reset them
  if (!verbose) {
    pbapply::pboptions(type = "none")
  }

  # Trim start date and end date to range of actual games
  schedule <- download_schedule(start_date, end_date, level = "MLB", game_type = game_type)
  start_date <- min(schedule$date)
  end_date <- max(schedule$date)

  base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details"
  gt_filter <- glue::glue("hfGT={paste0(game_type, '%7C', collapse = '')}")

  # Split the dates into 5-day chunks. The Savant API will return at most 25,000 rows. Assuming 300
  # pitches per game, a day with 15 games will have 4,500 pitches. We can safely download 5 days of
  # data, but more days would risk hitting the 25,000-row limit.
  days <- as.numeric(as.Date(end_date) - as.Date(start_date))
  payload <- tibble::tibble(start = as.Date(start_date) + seq(from = 0, by = 5, to = days)) |>
    dplyr::mutate(end = pmin(start + 4, end_date),) |>
    dplyr::cross_join(dplyr::distinct(schedule, date)) |>
    dplyr::filter(date >= start, date <= end) |>
    dplyr::distinct(start, end) |>
    dplyr::mutate(
      date_filter = glue::glue("game_date_gt={start}&game_date_lt={pmin(start + 4, end)}"),
      url = glue::glue("{base_url}&{gt_filter}&{date_filter}")
    )

  # The baseballsavant API can take some time to respond (often over 1 minute). To speed up this
  # process, we submit all requests right away without waiting for a response. The API gets to work
  # on these queries and returns them the next time we ask for them (later on in the script).
  if (verbose) {
    message("Submitting initial API requests...")
  }
  initial_request <- pbapply::pblapply(
    X = payload$url,
    FUN = function(url) {
      try(httr::GET(url, httr::timeout(1)), silent = TRUE)
    },
    cl = cl
  )

  if (verbose) {
    message(glue::glue("Attempting to download {nrow(payload)} payload(s)..."))
  }

  # Initialize our list of data payloads, treating all of them as errors before successful download
  data_list <- lapply(X = payload$url, FUN = function(x) return("error"))
  names(data_list) <- payload$url
  is_error <- sapply(data_list, FUN = function(x) identical(x, "error"))

  while(any(is_error)) {

    data_list[payload$url[is_error]] <- pbapply::pblapply(
      X = payload$url[is_error],
      FUN = function(url) {
        data <- try(
          expr = httr::content(
            httr::GET(url),
            type = "text/csv",    # requires readr::read_csv
            encoding = "UTF-8",
            name_repair = "universal_quiet",
            show_col_types = FALSE
          ),
          silent = TRUE
        )
        if ("try-error" %in% class(data)) {
          return("error")
        } else if (ncol(data) == 1) {
          # In Nov 2024, the API changed to return a 524 timeout message in the event of a timeout,
          # which does not throw an error for httr::content. This condition handles this case.
          return("error")
        } else if (nrow(data) == 0) {
          # This condition can happen when the date range includes no tracked games
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

  rows_per_payload <- sapply(
    X = data_list,
    FUN = function(x) {
      if (is.null(x)) {
        return(0)
      } else {
        return(nrow(x))
      }
    }
  )

  if (any(rows_per_payload == 25000)) {
    # The API limits the number of runs returned to 25,000 without throwing a warning,
    # so it's possible to get a lot less data than expected for the date range specified.
    sus_data <- sum(sapply(data_list, nrow) == 25000)
    warning(
      glue::glue("{sus_data} payload(s) returned exactly 25,000 rows. Data are likely missing.")
    )
  }

  data_combined <- try(do.call(rbind, args = data_list), silent = TRUE)

  if ("try-error" %in% class(data_combined)) {
    warning("Something went wrong when combining payloads. Returning list of payloads.")
    return(data_list)
  }
 
  data <- data_combined |>
    tibble::as_tibble() |>
    # re-define columns as needed to match statsapi
    dplyr::mutate(
      event_index = at_bat_number - 1,
      half_inning = dplyr::case_when(
        inning_topbot == "Top" ~ "top",
        inning_topbot == "Bot" ~ "bottom"
      ),
      # Convert bat speed, swing length and arm angle to numeric without throwing warnings
      bat_speed = convert_numeric(bat_speed),
      swing_length = convert_numeric(swing_length),
      arm_angle = convert_numeric(arm_angle)
    ) |>
    # re-order and re-name columns (but don't drop any)
    dplyr::select(
      # pitch identifiers (for joining on pitch table from statsapi)
      game_id = game_pk,
      game_date,
      game_type,
      year = game_year,
      event_index,
      pitch_number,
      # player and team identifiers
      home_team,
      away_team,
      batter_id = batter,
      bat_side = stand,
      batter_name = player_name,
      pitcher_id = pitcher,
      pitch_hand = p_throws,
      pre_runner_1b_id = on_1b,
      pre_runner_2b_id = on_2b,
      pre_runner_3b_id = on_3b,
      fielder_2_id = fielder_2,
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
      arm_angle,
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
