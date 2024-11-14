#' Convert string with NA values to numeric
#' 
#' The base function as.numeric throws a warning when coercion introduces NA. This function avoids
#' that warning by carefully handling expected NA strings.
#' 
#' @param string a character vector of strings to convert to numeric
#' @param na_string a character vector of strings expected to be coerced to NA, defaults to "--"
#' 
#' @return a numeric vector with NAs replacing strings expected to be coerced to NA
#' 
convert_numeric <- function(string, na_string = "--") {
  numeric_inf <- as.numeric(ifelse(string %in% na_string, "Inf", string))
  numeric <- ifelse(is.finite(numeric_inf), numeric_inf, NA)
  return(numeric)
}
