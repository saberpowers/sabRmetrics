#' Sanitize game type
#' 
#' Check that a character vector of game types are all valid game types.
#' 
#' @param game_type character vector of game types. Valid game types are:
#' \itemize{
#'   \item{"R" for Regular Season}
#'   \item{"F" for First-Round Playoff Series (aka wild card)}
#'   \item{"D" for Division Series (or minor-league quarter-finals)}
#'   \item{"L" for League Championship Series (or minor league semi-finals)}
#'   \item{"W" for World Series (or minor league championship)}
#'   \item{"A" for All-Star Game}
#'   \item{"E" for Exhibition}
#' }
#' 
#' @return simply returns the input vector `game_type` but throws error if invalid types are present
#' 
sanitize_game_type <- function(game_type) {
  game_type <- match.arg(
    arg = game_type,
    choices = c("R", "F", "D", "L", "W", "S", "A", "E"),
    several.ok = TRUE
  )
  return(game_type)
}
