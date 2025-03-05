#' Download player table
#' 
#' Download player information for a vector of years and a vector of levels.
#' 
#' @param year vector of integers (first valid year is 1876)
#' @inheritParams sanitize_level
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' 
#' @return a table of player data, indexed by player_id
#' 
#' @export
#' 
download_player <- function(year,
                            level = c("MLB", "AAA", "AA", "A+", "A", "CL", "DSL"),
                            cl = NULL) {

  level <- sanitize_level(level)
  sport_id <- get_sport_id(level)

  args_table <- expand.grid(
    year = year,
    sport_id = sport_id,
    stringsAsFactors = FALSE
  ) |>
    dplyr::distinct()

  args_list <- split(args_table, f = 1:nrow(args_table))

  player_list <- pbapply::pblapply(
    X = args_list,
    FUN = function(args) {
      base_url <- "https://statsapi.mlb.com/api/v1/sports"
      url <- glue::glue("{base_url}/{args$sport_id}/players?season={args$year}")
      stats_player <- jsonlite::fromJSON(url, flatten = TRUE)$people
      return(stats_player)
    },
    cl = cl
  )

  player <- do.call(dplyr::bind_rows, args = player_list) |>
    # we prefer dplyr::bind_rows over rbind because the number of columns can differ by year-level
    tibble::as_tibble() |>
    dplyr::transmute(
      player_id = id,
      name_full = fullName,
      name_last = useLastName,
      name_first = useName,
      name_given = firstName,
      name_middle = middleName,
      name_nickname = nickName,
      name_matrilineal = nameMatrilineal,
      name_pronounce = pronunciation,
      birth_date = birthDate,
      birth_city = birthCity,
      birth_state_prov = birthStateProvince,
      birth_country = birthCountry,
      draft_year = draftYear,
      mlb_debut_date = mlbDebutDate,
      bat_side = batSide.code,
      throw_hand = pitchHand.code,
      height = height,
      weight = weight,
      sz_top = strikeZoneTop,
      sz_bot = strikeZoneBottom,
      primary_position = primaryPosition.code
    ) |>
    dplyr::distinct()

  return(player)
}
