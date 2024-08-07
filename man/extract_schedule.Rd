% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_schedule.R
\name{extract_schedule}
\alias{extract_schedule}
\title{Extract schedule}
\usage{
extract_schedule(start_date, end_date, level = c("mlb", "aaa"))
}
\arguments{
\item{start_date}{first date included in the download}

\item{end_date}{last date included in the download}

\item{level}{character string, "mlb" (default) or "aaa"}
}
\value{
a dataframe of games, with columns \code{game_id}, \code{year}, \code{date}, \code{team_id_away}, \code{team_id_home}, \code{venue_id}
}
\description{
Extract a table of MLB or AAA games within a specified date range (must be same calendar year).
}
