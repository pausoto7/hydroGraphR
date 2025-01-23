#'  Creates a df with each location and the date range of the data 
#'
#' @description
#' `get_date_ranges` Uses the location names and the combined hydrologic data to 
#' create and then output a df that contains the date ranges for each location
#' 
#' @param all_hydro_sites_combined df of all the hydrology data  
#'
#'
#' @return A dataframe with updated information on the date range of each location
#' @export


get_date_ranges <- function(all_hydro_sites_combined){
  
  unique_locations_years <- all_hydro_sites_combined %>%
    dplyr::select(Parameter, STATION_NUMBER, unique_yrs) %>%
    dplyr::distinct() 
  
  param_list <- list()
  
  for (rows in 1:nrow(unique_locations_years)){
    

    LocationYearsOnlyParam <- unique_locations_years[rows, ]
    
    # Ensure the years are numeric
    locations_unique_years <- unlist(LocationYearsOnlyParam$unique_yrs)
    
    
    # Create year range from list of years
    ranges <- split(locations_unique_years, cumsum(c(1, diff(locations_unique_years) != 1)))
    
    
    year_ranges <- sapply(ranges, function(x) {
      if (length(x) == 1) {
        as.character(x)
      } else {
        paste0(min(x), "-", max(x))
      }
    })

    LocationYearsOnlyParam$year_ranges <- paste((unname(year_ranges)), collapse = ", ")
    
    LocationYearsOnlyParam <- LocationYearsOnlyParam %>%
      dplyr::select(Parameter, STATION_NUMBER, year_ranges) %>%
      dplyr::distinct()
    
    param_list[[rows]] <- LocationYearsOnlyParam
    
    
  }
  
  ranges_df <- do.call(rbind, param_list)
  
  return(ranges_df)
  
}

