#' Download player stats season summary
#' 
#' This function this the https://bdfed.stitch.mlbinfra.com/bdfed/stats/player api to download
#' player season summary stats. Coverage goes back to 1901 for MLB, 1990 for domestic minor leagues
#' and 2016 for the Dominican Summer League. Year and level can be provided as vector arugments.
#' 
#' @param year integer vector of years
#' @inheritParams sanitize_level
#' @param game_type "R" for regular season or "P" for playoffs (cannot be both)
#' @param position "hitting" or "pitching" (cannot be both)
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' 
#' @return a table of season summary statistics indexed by year, level, league_id and player_id
#' 
#' @export
#' 
download_season_summary <- function(year,
                                    level = "MLB",
                                    position = c("hitting", "pitching"),
                                    game_type = c("R", "P"),
                                    cl = NULL) {

  level_vec <- sanitize_level(level)
  position <- match.arg(position)
  game_type <- match.arg(game_type)

  league_level_by_year <- get_league_level_by_year(year)

  args_table <- expand.grid(
    year = year,
    level = level_vec,
    stringsAsFactors = FALSE
  ) |>
    dplyr::inner_join(league_level_by_year, by = c("level", "year")) |>
    dplyr::transmute(
      stitch_env = "prod",
      stats = "season",
      sortStat = "numberOfPitches",   # API call doesn't seem to work correctly if sortStat is blank
      limit = as.character(10000),    # a number that is hopefully much larger than ever necessary
      group = position,
      gameType = game_type,
      season = as.character(year),
      leagueIds = as.character(league_id)
    )

  args_list <- split(args_table, f = 1:nrow(args_table))

  season_summary_list <- pbapply::pblapply(
    X = args_list,
    FUN = function(args) {
      base_url <- "https://bdfed.stitch.mlbinfra.com/bdfed/stats/player"
      url <- glue::glue("{base_url}?{paste0(names(args), '=', unlist(args), collapse = '&')}")
      stats_player <- jsonlite::fromJSON(url, flatten = TRUE)$stats
      return(stats_player)
    },
    cl = cl
  )

  season_summary <- do.call(dplyr::bind_rows, args = season_summary_list) |>
    # we prefer dplyr::bind_rows over rbind because the number of columns can differ by year-level
    tibble::as_tibble() |>
    dplyr::rename(player_id = playerId, league_id = leagueId) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::left_join(league_level_by_year, by = c("year", "league_id")) |>
    dplyr::select(year, level, league_id, player_id, dplyr::everything())

  return(season_summary)
}
