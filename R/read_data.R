#' easy readers for tidyagronomy
#' Do you want to contribute? Submit a pull request!

#' @export
read_era5 <- NULL
read_era5 <- function(filePath){
  tidync::tidync(x = filePath) %>%
    hyper_tibble %>%
    dplyr::mutate(time = lubridate::as_datetime(time*60*60, origin = '1900-01-01 00:00:00.0' ))
}


