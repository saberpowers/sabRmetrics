% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_numeric.R
\name{convert_numeric}
\alias{convert_numeric}
\title{Convert string with NA values to numeric}
\usage{
convert_numeric(string, na_string = "--")
}
\arguments{
\item{string}{a character vector of strings to convert to numeric}

\item{na_string}{a character vector of strings expected to be coerced to NA, defaults to "--"}
}
\value{
a numeric vector with NAs replacing strings expected to be coerced to NA
}
\description{
The base function as.numeric throws a warning when coercion introduces NA. This function avoids
that warning by carefully handling expected NA strings.
}
