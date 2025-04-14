#' Calculate Cumulative Degree Units
#'
#' @description
#' Computes the cumulative sum of a daily degree unit column (e.g., daily_gdu)
#' grouped by specified variables (typically location and year). This helps track
#' the progress of thermal time accumulation throughout a growing season.
#'
#' @param data A data frame containing daily measurement data.
#' @param unit_col A string naming the column that contains the daily thermal unit values.
#' @param group_vars A character vector defining the grouping variables (default: c("locationID", "Year")).
#' @param cum_col A string for the name of the output cumulative column. If NULL, the function creates
#'                a name by prefixing unit_col with "cumulative_". Default is NULL.
#'
#' @return A data frame with a new column containing the cumulative sum of the specified thermal unit.
#'
#' @details
#' The function uses dplyr's grouping and mutate to calculate a running total within each group.
#'
#' @examples
#' df <- data.frame(
#'   locationID = rep("A", 5),
#'   Year = rep(2025, 5),
#'   time = as.Date('2025-04-01') + 0:4,
#'   daily_gdu = c(0, 2, 5, 3, 4)
#' )
#' cumulative_degree_units(df, unit_col = "daily_gdu")
#'
#' @export
cumulative_degree_units <- function(data, unit_col, group_vars = c("locationID", "Year"), cum_col = NULL) {
  if (is.null(cum_col)) {
    cum_col <- paste0("cumulative_", unit_col)
  }
  data %>%
    group_by(across(all_of(group_vars))) %>%
    mutate("{cum_col}" := cumsum(.data[[unit_col]])) %>%
    ungroup()
}
