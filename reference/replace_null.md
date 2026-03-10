# Replace NULL value

Replace NULL value with a specified replacement value, useful when an
API endpoint may not return a value for a given field.

## Usage

``` r
replace_null(x, replacement = NA)
```

## Arguments

- x:

  the value to replace if NULL

- replacement:

  the value with which to replace NULL, defaults to NA

## Value

If x is NULL, return replacement, else return x
