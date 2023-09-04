#' easy readers for TidyAgronomy
#' Do you want to contribute? Submit a pull request! 

#' @export
easy_era5 <- NULL
easy_era5 <- function(filePath){
  tidync(x = filePath) %>%
    hyper_tibble %>% 
    mutate(time = as_datetime(time*60*60, origin = '1900-01-01 00:00:00.0' ))
}


