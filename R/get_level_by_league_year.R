#' Get league level by year
#' 
#' The minor leagues have gone through significant restructuring a couple of times (1990 and 2020),
#' so we can't statically map each league to a level. This function takes a vector of years and
#' returns a mapping from league_id to level for each year.
#' 
#' @param year integer vector of years
#' 
#' @return a table of `level` indexed by `league_id` and `year`
#' 
#' @export
#' 
get_league_level_by_year <- function(year) {

  league_level <- tibble::tribble(
    ~league_id, ~league_name,               ~year_classified,  ~level,
    103,        "American League",          1901,              "MLB",
    104,        "National League",          1901,              "MLB",
    109,        "Texas League",             1990,              "AA",
    110,        "California League",        1990,              "A+",
    110,        "California League",        2020,              "A",
    111,        "Southern League",          1990,              "AA",
    112,        "Pacific Coast League",     1990,              "AAA",
    113,        "Eastern League",           1990,              "AA",
    116,        "South Atlantic League",    1990,              "A",
    116,        "South Atlantic League",    2020,              "A+",
    117,        "International League",     1990,              "AAA",
    118,        "Midwest League",           1990,              "A",
    118,        "Midwest League",           2020,              "A+",
    120,        "Appalachian League",       1990,              "SS",
    120,        "Appalachian League",       2020,              NA,
    121,        "Arizona Complex League",   1990,              "CL",
    122,        "Carolina League",          1990,              "A+",
    122,        "Carolina League",          2020,              "A",
    123,        "Florida State League",     1990,              "A+",
    123,        "Florida State League",     2020,              "A",
    124,        "Florida Complex League",   1990,              "CL",
    126,        "Northwest League",         1990,              "SS",
    126,        "Northwest League",         2020,              "A+",
    127,        "New York-Penn League",     1990,              "SS",
    127,        "New York-Penn League",     2020,              NA,
    128,        "Pioneer League",           1990,              "SS",
    128,        "Pioneer League",           2020,              NA,
    130,        "Dominican Summer League",  1990,              "DSL",
  )

  league_for_level_year <- tibble::tibble(year = unique(year)) |>
    dplyr::cross_join(league_level) |>
    # For each year, get the most up-to-date classification for each league
    dplyr::filter(year_classified <= year) |>
    dplyr::group_by(league_id, year) |>
    dplyr::arrange(-year_classified) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    # NA level means that the league is no longer part of the affiliated minor leagues
    dplyr::filter(!is.na(level)) |>
    dplyr::select(league_id, year, level) |>
    dplyr::arrange(league_id, year, level)
}
