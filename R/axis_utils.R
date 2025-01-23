#'  Creates a custom seq for axis for plotting
#'
#' @description
#' `get_custom_plot_seq` uses custom_ymax, custom_ymin inputs to create appropriate
#' sequence of numbers.
#' 
#' @param custom_ymax ymax of data
#' @param custom_ymin ymin of data 
#'
#' @return A sequence of numbers to be used for x or y axis on a plot


get_custom_plot_seq <- function(custom_ymax, custom_ymin) {
  
  # Return NA if either custom_ymax or custom_ymin is NA
  if (is.na(custom_ymax) || is.na(custom_ymin)) {
    return(NA)
  }
  
  # Calculate the difference
  max_min_diff <- custom_ymax - custom_ymin
  
  # Define intervals based on the range
  interval <- if (max_min_diff <= 2) {
    0.5
  } else if (max_min_diff <= 4) {
    1
  } else if (max_min_diff <= 20) {
    2
  } else if (max_min_diff <= 50) {
    5
  } else if (max_min_diff <= 100) {
    10
  } else if (max_min_diff <= 200) {
    20
  } else if (max_min_diff <= 500) {
    50
  } else if (max_min_diff <= 1000) {
    100
  } else if (max_min_diff <= 1500) {
    150
  } else if (max_min_diff <= 2000) {
    200
  } else if (max_min_diff <= 3000) {
    500
  } else if (max_min_diff <= 5500) {
    1000
  } else {
    2000
  }
  
  # Adjust ymin and ymax by a buffer if the range is very small
  buffer <- if (max_min_diff < 2) 0.5 else 0
  custom_ymin <- custom_ymin - buffer
  custom_ymax <- custom_ymax + buffer
  
  # Generate the sequence for breaks based on calculated interval
  break_seq <- seq(
    from = floor(custom_ymin / interval) * interval,
    to = ceiling(custom_ymax / interval) * interval,
    by = interval
  )
  
  return(break_seq)
}
