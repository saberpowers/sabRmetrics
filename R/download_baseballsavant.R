#' Download MLB data from baseballsavant.mlb.com
#' 
#' Loop over five days at a time to download data from the Statcast search API at Baseball Savant.
#' This includes swing tracking data not available through the MLB statsapi.
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' 
#' @return a dataframe with 94 columns and one row per pitch, with all available data
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
download_baseballsavant <- function(start_date, end_date, cl = NULL) {

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

  data_list <- pbapply::pblapply(
    X = start_date_seq,
    FUN = function(start_date) {
      end_date <- start_date + 4
      base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details"
      url <- glue::glue("{base_url}&game_date_gt={start_date}&game_date_lt={end_date}")
      data <- read.csv(url(url))
      if (nrow(data) == 25000) {
        warning(
          glue::glue("Exactly 25,000 rows returned for {start_date} to {end_date}")
        )
      }
      return(data)
    },
    cl = cl
  )

  data <- do.call(dplyr::bind_rows, args = data_list)

  return(data)
}
