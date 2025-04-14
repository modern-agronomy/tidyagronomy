#' Identify Threshold Crossing Date for Cumulative Thermal Units
#'
#' @description
#' Identifies, within groups (e.g., by location and year), the first date on which the cumulative
#' thermal unit value exceeds a given threshold. This is particularly useful for predicting phenological events.
#'
#' @param data A data frame that contains cumulative thermal unit data and a date column.
#' @param cumulative_col A string specifying the column name with cumulative values (e.g., "cumulative_daily_gdu").
#' @param threshold A numeric value representing the threshold level to be exceeded (e.g., 2000).
#' @param date_col A string specifying the name of the date column (default: "time").
#' @param group_vars A character vector of column names for grouping (default: c("locationID", "Year")).
#'
#' @return A data frame with one row per group that reports the first date the cumulative value exceeds the threshold.
#'
#' @details
#' The function filters for rows where the cumulative value meets or exceeds the threshold,
#' then selects the earliest date per group.
#'
#' @examples
#' df <- data.frame(
#'   locationID = rep("A", 5),
#'   Year = rep(2025, 5),
#'   time = as.Date('2025-04-01') + 0:4,
#'   cumulative_daily_gdu = c(0, 2, 7, 10, 15)
#' )
#' threshold_cross_date(df, cumulative_col = "cumulative_daily_gdu", threshold = 8)
#'
#' @export
threshold_cross_date <- function(data, cumulative_col, threshold, date_col = "time", group_vars = c("locationID", "Year")) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    filter(.data[[cumulative_col]] >= threshold) %>%
    slice_min(order_by = .data[[date_col]], with_ties = FALSE) %>%
    ungroup()
}
