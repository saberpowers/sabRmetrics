#' Extract fielding lineup
#' 
#' Extract the starting fielding lineup from a provided players JSON object
#' 
#' @param players a players JSON object extracted from a boxscore JSON object
#' 
#' @return a table of player_id keyed on position (2-10)
#' 
extract_fielding_lineup <- function(players) {

  first_position <- do.call(
    what = c,
    args = lapply(players, function(x) x$allPositions$code[1])
  ) |>
    as.matrix() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::transmute(
      position = as.integer(V1),
      player_id = as.integer(substring(rowname, 3))
    )

  is_substitute <- sapply(players, function(x) x$gameStatus)['isSubstitute', ] |>
    sapply(identity) |>
    as.matrix() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::transmute(
      player_id = as.integer(substring(rowname, 3)),
      is_substitute = V1
    )

  fielding_lineup <- first_position |>
    dplyr::left_join(is_substitute, by = "player_id") |>
    dplyr::filter(position != 1, !is_substitute) |>
    dplyr::select(position, player_id) |>
    dplyr::arrange(position)

  return(fielding_lineup)
}