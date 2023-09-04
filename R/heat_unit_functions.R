#' Growing Degree & Stress Calculations
#' References: https://products.climate.ncsu.edu/ag/corn/about/

#' @export
growing_degree_unit <- NULL
growing_degree_unit <- function(value, baseline, upper_bound = NULL) {
  
  if (is.null(upper_bound)) {
    max(value - baseline, 0)
  } else {
    max(min(value, upper_bound) - baseline, 0)
  }
  
}

#' @export
stress_degree_unit <- NULL
stress_degree_unit <- function(value, upper_bound) {
  max(value, upper_bound) - upper_bound
}

#' @export
chill_degree_units <- NULL
#' TODO