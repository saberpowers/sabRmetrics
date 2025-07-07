#' Download schedule
#' 
#' Download a table of MLB or AAA games within a specified date range (must be same calendar year).
#' 
#' @param start_date first date included in the download
#' @param end_date last date included in the download
#' @inheritParams sanitize_level
#' @inheritParams sanitize_game_type
#'
#' @return a dataframe of games, with columns `game_id`, `year`, `date`, `team_id_away`, `team_id_home`, `venue_id`
#' 
#' @export
#' 
download_schedule <- function(start_date,
                              end_date,
                              level = c("MLB", "AAA", "AA", "A+", "A"),
                              game_type = "R") {

  if (lubridate::year(start_date) != lubridate::year(end_date)) {
    stop("Please choose `start_date` and `end_date` within the same calendar year")
  }
  level <- match.arg(level)
  game_type <- sanitize_game_type(game_type)

  start <- format(as.Date(start_date), "%m/%d/%Y")
  end <- format(as.Date(end_date), "%m/%d/%Y")
  sport_id <- get_sport_id(level)
  schedule_filter <- glue::glue(
    "sportId={sport_id}&gameType={paste(game_type, collapse = ',')}&startDate={start}&endDate={end}"
  )
  endpoint <- glue::glue("http://statsapi.mlb.com:80/api/v1/schedule?{schedule_filter}")

  schedule_json <- jsonlite::fromJSON(endpoint, flatten = TRUE)
  if(schedule_json$totalGames == 0) {
    stop(
      glue::glue(
        "No games found between {start} and {end} of type {paste(game_type, collapse = ', ')}"
      )
    )
  }

  schedule <- do.call(dplyr::bind_rows, args = schedule_json$dates$games)

  # We rely on the resumeDate column to avoid duplicating resumed games, but that column will
  # not be included in `schedule` if there were no resumed games in our timeframe.
  if (is.null(schedule$resumeDate)) {
    schedule$resumeDate <- NA
  }

  game <- schedule |>
    # Filter out non-NA resumeDate to get down to one row per game ID
    dplyr::filter(status.detailedState %in% c("Final", "Completed Early"), is.na(resumeDate)) |>
    dplyr::arrange(officialDate) |>
    dplyr::select(
      game_id = gamePk,
      game_type = gameType,
      year = season,
      date = officialDate,
      venue_id = venue.id,
      team_id_away = teams.away.team.id,
      team_name_away = teams.away.team.name,
      score_away = teams.away.score,
      team_id_home = teams.home.team.id,
      team_name_home = teams.home.team.name,
      score_home = teams.home.score
    )
  
  return(game)
}
