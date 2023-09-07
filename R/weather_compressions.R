#' Compress hourly weather using a variety of methods.
#' The first is converting hourly to "mean daily"
#'
#' TODO: citation
#' the user is left to fill in the details if they wish to change the aggregation method


#' @export
mean_daily <- NULL
mean_daily <- function(.data,
                       location_id_column,
                       season_column,
                       time_column,
                       temp_column) {
  .data %>%
    group_by(!!rlang::ensym(location_id_column), !!rlang::ensym(season_column)) %>%
    timetk::summarise_by_time(.date_var = !!rlang::ensym(time_column),
                      .by = "day",
                      mean_daily_temp = mean(!!rlang::ensym(temp_column))
    )

}

