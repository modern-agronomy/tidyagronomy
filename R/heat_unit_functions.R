#' Growing Degree & Stress Calculations
#' References: https://products.climate.ncsu.edu/ag/corn/about/

#' Calculate Daily Growing Degree Units (GDU)
#'
#' @description
#' Computes the daily Growing Degree Unit (GDU) based on the daily mean temperature.
#' The function subtracts a baseline temperature from the daily temperature, while capping
#' the effective temperature at an upper bound. If the daily temperature does not exceed the baseline,
#' GDU is zero.
#'
#' @param temp A numeric vector of daily mean temperatures (in degrees Celsius).
#' @param baseline A numeric value for the lower threshold (default is 10°C).
#' @param upper_bound A numeric value for the upper cap (default is 30°C).
#'
#' @return A numeric vector of daily GDU values.
#'
#' @details
#' The formula used is:
#' \deqn{GDU = \max(\min(temp, \; upper\_bound) - baseline, \; 0)}
#'
#' @examples
#' # For a vector of daily temperatures
#' temps <- c(8, 12, 25, 35)
#' growing_degree_unit(temps)
#'
#' @export
growing_degree_unit <- function(temp, baseline = 10, upper_bound = 30) {
  temp <- as.numeric(temp)
  effective_temp <- pmin(temp, upper_bound)
  gdu <- ifelse(effective_temp > baseline, effective_temp - baseline, 0)
  return(gdu)
}

#' Calculate Daily Stress Degree Units (SDU)
#'
#' @description
#' Computes the daily Stress Degree Unit (SDU) by measuring the deviation of the daily temperature
#' from an optimal range. If the temperature is below a lower optimum (when provided) or above an upper optimum,
#' the function returns the corresponding deviation. When the temperature falls within the optimal range,
#' no stress is accumulated (i.e., zero). If `lower_opt` is set to NULL, cold stress is not computed.
#'
#' @param temp A numeric vector of daily mean temperatures (in degrees Celsius).
#' @param lower_opt A numeric value defining the lower optimum temperature (default is 10°C).
#'                  Set to NULL to skip lower stress computation.
#' @param upper_opt A numeric value defining the upper optimum temperature (default is 30°C).
#'
#' @return A numeric vector of daily SDU values.
#'
#' @examples
#' # For a vector of daily temperatures with default lower_opt:
#' temps <- c(7, 15, 33)
#' stress_degree_unit(temps)
#'
#' # For a vector of daily temperatures with no lower stress calculation:
#' stress_degree_unit(temps, lower_opt = NULL, upper_opt = 30)
#'
#' @export
stress_degree_unit <- function(temp, lower_opt = 10, upper_opt = 30) {
  temp <- as.numeric(temp)
  if (is.null(lower_opt)) {
    # Only calculate stress when temperature exceeds the upper optimum
    stress <- ifelse(temp > upper_opt, temp - upper_opt, 0)
  } else {
    stress <- ifelse(temp < lower_opt, lower_opt - temp,
                     ifelse(temp > upper_opt, temp - upper_opt, 0))
  }
  return(stress)
}

#' Calculate Daily Chilling Degree Units (CDU)
#'
#' @description
#' Computes the daily Chilling Degree Unit (CDU) to assess cold exposure. It calculates the difference
#' between a specified chilling threshold and the daily temperature, only when the temperature is below that threshold.
#' If the temperature is above the threshold, the chilling unit is zero.
#'
#' @param temp A numeric vector of daily mean temperatures (in degrees Celsius).
#' @param chilling_threshold A numeric value for the threshold (default is 7°C).
#'
#' @return A numeric vector of daily CDU values.
#'
#' @examples
#' # For a vector of daily temperatures
#' temps <- c(4, 8, 6)
#' chilling_degree_unit(temps)
#'
#' @export
chilling_degree_unit <- function(temp, chilling_threshold = 7) {
  temp <- as.numeric(temp)
  cdu <- ifelse(temp < chilling_threshold, chilling_threshold - temp, 0)
  return(cdu)
}

#' will clean this up later.

#' #' @export
#' growing_degree_unit <- NULL
#' growing_degree_unit <- function(value, baseline, upper_bound = NULL) {
#'
#'   if (is.null(upper_bound)) {
#'     max(value - baseline, 0)
#'   } else {
#'     max(min(value, upper_bound) - baseline, 0)
#'   }
#'
#' }
#'
#' #' @export
#' stress_degree_unit <- NULL
#' stress_degree_unit <- function(value, upper_bound) {
#'   max(value, upper_bound) - upper_bound
#' }
#'
#' #' @export
#' chill_degree_units <- NULL
#' #' TODO
