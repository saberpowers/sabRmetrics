% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_statsapi.R
\name{download_statsapi}
\alias{download_statsapi}
\title{Download data from statsapi.mlb.com}
\usage{
download_statsapi(
  start_date,
  end_date,
  level = "MLB",
  game_type = "R",
  cl = NULL
)
}
\arguments{
\item{start_date}{first date included in the download}

\item{end_date}{last date included in the download}

\item{level}{character vector of levels. Valid levels are:
\itemize{
\item{"MLB" for Major League Baseball}
\item{"AAA" for Triple-A}
\item{"AA" for Double-A}
\item{"A+" for High-A}
\item{"A" for Single-A}
\item{"SS" (defunct since 2020) for Short-Season A and Rookie Advanced}
\item{"CL" for Complex Leagues}
\item{"DSL" for Dominican Summer League}
}}

\item{game_type}{character vector of game types to include. Options are "R" (regular sesason),
"F" (first-round playoff series, aka wild card), "D" (division series), "L" (league
championship series), "W" (world series), "S" (spring training), "A" (all-star game),
"E" (exhibition). Default is "R".}

\item{cl}{optional cluster object for parallel computation, default is NULL (not parallel)}
}
\value{
a list of four dataframes: \code{event}, \code{pitch}, \code{play} and game
}
\description{
Download data from all \emph{regular season} MLB games within a specified date range.
}
\examples{
\dontrun{
   data_statsapi <- download_statsapi(
     start_date = "2024-07-01",
     end_date = "2024-07-01"
   )
}

}
