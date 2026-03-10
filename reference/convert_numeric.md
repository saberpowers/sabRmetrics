# Convert string with NA values to numeric

The base function as.numeric throws a warning when coercion introduces
NA. This function avoids that warning by carefully handling expected NA
strings.

## Usage

``` r
convert_numeric(string, na_string = "--")
```

## Arguments

- string:

  a character vector of strings to convert to numeric

- na_string:

  a character vector of strings expected to be coerced to NA, defaults
  to "–"

## Value

a numeric vector with NAs replacing strings expected to be coerced to NA
