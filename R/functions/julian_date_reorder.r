
valiDATE <- function(date) {
  stopifnot(`date must take the form of "MM-DD"` = 
              stringr::str_detect(date, "^\\d{2}-\\d{2}$"))
}

days <- function(x, end = NULL) {
  valiDATE(end)
  
  calcdiff <- function(x) {
    if (is.null(end)) {end <- c("04-30")
    }
    endx <- glue::glue("{lubridate::year(x)}-{end}")
    if(lubridate::yday(x) > lubridate::yday(endx)) {
      diff <- ceiling(difftime(x, endx, units = "days"))
    } else {
      endx <- glue::glue("{lubridate::year(x)-1}-{end}")
      diff <- ceiling(difftime(x, endx, units = "days"))
    }
    unclass(diff)
  }
  
  purrr::map_dbl(x, calcdiff)
}