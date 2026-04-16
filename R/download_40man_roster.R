#' Download MLB 40-Man Rosters
#'
#' Retrieves the 40-man roster for all Major League Baseball teams for a specified date
#' using the MLB Stats API.
#'
#' @param date A character string representing the date in \code{YYYY-MM-DD} format. 
#'   Defaults to the current system date.
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#'
#' @return A \code{tibble} containing the following columns:
#' \itemize{
#'   \item \strong{team_id}: The unique numeric identifier for the MLB team.
#'   \item \strong{player_id}: The unique numeric identifier for the player.
#'   \item \strong{status}: The character code representing the player's current roster status.
#'     Possible values include:
#'     \itemize{
#'       \item A: Active
#'       \item BRV: Bereavement List
#'       \item D{#}: Injured {#}-Day
#'       \item PL: Paternity List
#'       \item RM: Reassigned to Minors
#'       \item SU: Suspended
#'     }
#' }
#'
#' @importFrom lubridate year
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom pbapply pblapply
#' @importFrom dplyr mutate bind_rows rename select
#' 
#' @export
download_40man_roster <- function(date = Sys.Date(), cl = NULL) {

  year <- lubridate::year(date)
  endpoint_team <- glue::glue("https://statsapi.mlb.com/api/v1/teams?sportIds=1&season={year}")
  team <- jsonlite::fromJSON(endpoint_team)$teams

  args_table <- tibble::tibble(
    team_id = team$id,
    date = date
  )

  args_list <- split(args_table, f = 1:nrow(args_table))

  roster_list <- pbapply::pblapply(
    X = args_list,
    FUN = function(args) {
      base_url <- "https://statsapi.mlb.com/api/v1/teams"
      url <- glue::glue("{base_url}/{args$team_id}/roster/40Man?date={args$date}")
      roster <- jsonlite::fromJSON(url, flatten = TRUE)$roster |>
        dplyr::mutate(team_id = args$team_id)
      return(roster)
    },
    cl = cl
  )

  roster <- do.call(dplyr::bind_rows, args = roster_list) |>
    tibble::as_tibble() |>
    dplyr::rename(player_id = person.id, status = status.code) |>
    dplyr::select(team_id, player_id, status)

  return(roster)
}
