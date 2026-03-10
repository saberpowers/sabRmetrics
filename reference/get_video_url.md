# Get the URL to watch the video of any play

Using `play_id` from the statsapi download, you can pull up the video of
any play

## Usage

``` r
get_video_url(play_id)
```

## Arguments

- play_id:

  character vector of play identifier from statsapi

## Value

character vector of URLs

## Examples

``` r
get_video_url("064e8918-e2b7-40f0-ac39-968932652154")
#> https://baseballsavant.mlb.com/sporty-videos?playId=064e8918-e2b7-40f0-ac39-968932652154
```
