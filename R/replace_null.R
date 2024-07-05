#' Replace NULL value
#' 
#' Replace NULL value with a specified replacement value, useful when an API endpoint may not
#' return a value for a given field.
#' 
#' @param x the value to replace if NULL
#' @param replacement the value with which to replace NULL, defaults to NA
#' 
#' @return 
#' If x is NULL, return replacement, else return x
#' 
replace_null <- function(x, replacement = NA) {
  if (is.null(x)) {
    return(replacement)
  } else {
    return(x)
  }
}
