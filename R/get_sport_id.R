#' Get Sport ID for a specified level
#' 
#' This function returned a vector of sport IDs corresponding to a vector of levels.
#' 
#' @return an integer vector of sport IDs
#' 
#' @inheritParams sanitize_level
#' 
get_sport_id <- function(level) {
  level <- sanitize_level(level)
  sport_id <- dplyr::case_when(
    level == "MLB" ~ 1,
    level == "AAA" ~ 11,
    level == "AA" ~ 12,
    level == "A+" ~ 13,
    level == "A" ~ 14,
    level %in% c("CL", "DSL") ~ 16
  )
  return(sport_id)
}
