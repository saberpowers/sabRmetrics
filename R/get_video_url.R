#' Get the URL to watch the video of any play
#' 
#' Using `play_id` from the statsapi download, you can pull up the video of any play
#' 
#' @param play_id character vector of play identifier from statsapi
#' 
#' @return character vector of URLs
#' 
#' @examples
#' get_video_url("064e8918-e2b7-40f0-ac39-968932652154")
#' 
#' @export
#' 
get_video_url <- function(play_id) {
  glue::glue("https://baseballsavant.mlb.com/sporty-videos?playId={play_id}")
}
