#' Download data from statsapi.mlb.com
#' 
#' Download data from all *regular season* MLB games within a specified date range.
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @param level character string, "mlb" (default) or "aaa"
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' 
#' @return a list of four dataframes: `event`, `pitch`, `play` and game
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
download_statsapi <- function(start_date, end_date, level = c("mlb", "aaa"), cl = NULL) {

  game <- extract_schedule(start_date, end_date, level)
  year <- lubridate::year(start_date)

  data_list <- pbapply::pblapply(
    X = game$game_id,
    # If we encounter an error, try a total of three times before returning NULL and moving on
    FUN = function(game_id) {
      is_success <- FALSE
      num_attempts <- 0
      while (!is_success & num_attempts < 3) {
        Sys.sleep(0.1)  # Avoid being rate-limited by statsapi
        data <- try(extract_game(game_id))
        if ("try-error" %in% class(data)) {
          num_attempts <- num_attempts + 1
          data <- NULL
          Sys.sleep(5)  # Take a long pause in case it helps avoid network error
        } else {
          is_success <- TRUE
        }
      }
      return(data)
    },
    cl = cl
  )

  event <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$event)) |>
    tibble::add_column(year = year, .after = "game_id")
  pitch <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$pitch)) |>
    tibble::add_column(year = year, .after = "game_id")
  play <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$play)) |>
    tibble::add_column(year = year, .after = "game_id")

  data <- list(
    event = event,
    pitch = pitch,
    play = play,
    game = game
  )
  
  return(data)
}
