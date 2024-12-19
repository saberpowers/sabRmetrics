
get_level_by_league_year <- function(year) {

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
    dplyr::filter(year_classified <= year) |>
    dplyr::group_by(league_id, year) |>
    dplyr::arrange(-year_classified) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(level)) |>
    dplyr::select(level, year, league_id) |>
    dplyr::arrange(level, year, league_id)
}
