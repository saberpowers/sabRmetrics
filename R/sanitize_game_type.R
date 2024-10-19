#' Sanitize game type
#' 
#' Check that a character vector of game types are all valid game types.
#' 
#' @param game_type character vector of game types
#' 
#' @return simply returns the input vector `game_type` but throws error if invalid types are present
#' 
sanitize_game_type <- function(game_type) {
  game_type <- match.arg(
    arg = game_type,
    choices = c(
      "R",  # regular season
      "F",  # first-round playoff series, aka wild card
      "D",  # division series
      "L",  # league championship series
      "W",  # world series
      "S",  # spring training
      "A",  # all-star game
      "E"   # exhibition
    ),
    several.ok = TRUE
  )
  return(game_type)
}
