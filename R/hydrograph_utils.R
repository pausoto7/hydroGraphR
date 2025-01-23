#' Filter data for hydrometric parameter to determine Y-axis Label
#'
#' @description
#' Filters hydrometric data based on the specified parameter (`"Flow"` or `"Level"`) 
#' and determines the corresponding y-axis label for hydrographs.
#'
#' @param all_hydro_sites_historical A data frame containing historical hydrometric data.
#' @param all_hydro_sites_1yr A data frame containing hydrometric data for the current year.
#' @param parameter A character string specifying the parameter to filter by. Options are `"Flow"` or `"Level"`.
#'
#' @return A list with the following components:
#' - `historical_data`: Filtered historical hydrometric data.
#' - `current_data`: Filtered current year hydrometric data.
#' - `y_axis_label`: A character string for the y-axis label, either `"Flow (m³/s)"` or `"Water Level (m)"`.
#'

select_hydro_parameter <- function(all_hydro_sites_historical, all_hydro_sites_1yr, parameter){
  
  
  if(tolower(parameter) == "flow"){
    
    all_hydro_sites_historical <- all_hydro_sites_historical %>%
      dplyr::filter(Parameter == "Flow")
    
    all_hydro_sites_1yr <- all_hydro_sites_1yr %>%
      dplyr::filter(Parameter == "Flow")
    
    y_lab <- expression(Discharge~(m^3/s))
    
  }else if(tolower(parameter) == "level"){
    
    all_hydro_sites_historical <- all_hydro_sites_historical %>%
      dplyr::filter(Parameter == "Level")
    
    all_hydro_sites_1yr <- all_hydro_sites_1yr %>%
      dplyr::filter(Parameter == "Level")
    
    y_lab <- "Water level (m)"
    
  }else{
    
    stop("Please input an appropriate parameter. Acceptable parameters are WL or flow.")
    
  }
  

  return(list(all_hydro_sites_historical, all_hydro_sites_1yr, y_lab) )
}



#' Choose Timeline for Hydrograph (Calendar Year or Water Year)
#'
#' @description
#' This function selects and filters hydrometric data based on whether the data should
#' be plotted by calendar year or water year.
#'
#' @param all_hydro_sites_historical Data frame of historical hydrometric data.
#' @param all_hydro_sites_1yr Data frame of current year hydrometric data.
#' @param WY Character string indicating whether to use calendar year ("no") or water year ("yes").
#' @param locations WSC locations
#' @param location_num number of iteration through loop
#'
#' @return A list containing the filtered historical and current year datasets, and the year label.
#' 

# Choose between water year (WY) and calendar year (CY) timelines
choose_hydro_timeline <- function(all_hydro_sites_historical, all_hydro_sites_1yr, WY, locations, location_num){
  
  
  if (WY == FALSE){
    
    all_hydro_sites_historical_filtered <- all_hydro_sites_historical %>%
      dplyr::filter(STATION_NUMBER == locations[location_num])
    
    all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr %>%
      dplyr::filter(STATION_NUMBER == locations[location_num])
    
    year_label <- unique(all_hydro_sites_1yr$year_col)
    
  }else{ 
    
    # rename arbitrary_date so it's plotted properly
    
    all_hydro_sites_historical_filtered <- all_hydro_sites_historical %>%
      dplyr::filter(STATION_NUMBER == locations[location_num]) %>%
      dplyr::select(-arbitrary_date) %>%
      dplyr::rename(arbitrary_date = arbitrary_date_WY)
    
    all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr %>%
      dplyr::filter(STATION_NUMBER == locations[location_num]) %>% 
      dplyr::select(-arbitrary_date) %>%
      dplyr::rename(arbitrary_date = arbitrary_date_WY)
    
    # Code to check what the current year is. If DF is empty will follow "else" path to avoid an error
    if (nrow(all_hydro_sites_1yr_filtered) > 0 && !all(is.na(all_hydro_sites_1yr_filtered$arbitrary_date))) {
      
      current_year <- lubridate::year(max(all_hydro_sites_1yr_filtered$Date, na.rm = TRUE))
      
    } else {
      warning("all_hydro_sites_1yr is empty. Historical values cannot be compared with this year")
      current_year <- NA  # Or any default value
    }      
    
    years_in_dataset <- unique(lubridate::year(all_hydro_sites_1yr_filtered$Date))
    
    previous_yr <- current_year -1
    
    if (!is.na(previous_yr) && !(previous_yr %in% years_in_dataset)){
      warning("Note that your current year data set does not include data from Oct-Dec. You may have selected plotting the data in water years. 
              You may want to run the previous function with WY = `yes` if you haven't already.")
    }
    
    year_label <- unique(all_hydro_sites_1yr$WY_col)
    
  }
  
  return(list(all_hydro_sites_historical_filtered, all_hydro_sites_1yr_filtered, year_label))
}  



#' Set Custom Limit for Plotting Axis
#'
#' This function calculates a custom limit for the y-axis (or any other axis) by taking
#' the maximum or minimum of two vectors, with a scaling multiplier. If both vectors
#' contain only `NA` values, the function returns `NA`. If a valid custom input is provided,
#' that value is returned instead of performing the calculation.
#'
#' @param custom_input Numeric. A custom limit input value. If `NA`, the function will calculate the limit.
#' @param vec1 Numeric vector. The first vector to be considered in the limit calculation.
#' @param vec2 Numeric vector. The second vector to be considered in the limit calculation.
#' @param multiplier Numeric. A scaling factor applied to the calculated limit (default is 1).
#' @param direction Character. Either "max" to calculate the maximum limit or "min" to calculate the minimum limit. Default is "max".
#'
#' @return Numeric. The calculated axis limit based on the input vectors and scaling factor, or the provided custom input.
#' If both vectors contain only `NA` values, the function returns `NA`.
#'

set_custom_limit <- function(custom_input, vec1, vec2, multiplier = 1, direction = "max") {
  if (is.na(custom_input)) {
    if (all(is.na(vec1)) && all(is.na(vec2))) {
      return(NA)  # Return NA or another fallback value if both vectors are all NA
    } else {
      if (direction == "max") {
        limit_num <- max(vec1, vec2, na.rm = TRUE)
        return(ceiling(limit_num * multiplier))
      } else if (direction == "min") {
        limit_num <- min(vec1, vec2, na.rm = TRUE)
        return(floor(limit_num * multiplier))
      }
    }
  } else {
    return(custom_input)  # Return the input if it's not NA
  }
}
