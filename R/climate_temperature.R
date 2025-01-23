#'  Wrangles temperature data
#'
#' @description 
#' `temperature_stats_daily` This function calculates temperature stats for the 
#' input length of time on a daily basis.
#' `temp_all_years_monthly` This function gives average temperature stats on a 
#' monthly basis. 
#' `temp_count_NAs` This function counts the amount of NA values in a dataset on
#' an annual basis
#'
#' @param climate_data climate data from climate station 
#' @param minYr The minimum year included in the historical plot calculations. 
#' Leave as NA if you want the minimum year in historical data.
#' @param maxYr The maximum year included in the historical plot calculations
#' Leave as NA if you want the maximum year present in historical data. Also 
#' consider what your YOI is and if you want that included or not.
#'
#' @return dataframe with statistics of air temperature data

#' @examples
#' #' # First, clean raw climate data using clean_climate()
#' #climate_clean <- clean_climate(climate_raw)
#' 
#' # Calculate temperature stats
#' #temperature_stats_daily(climate_clean, minYr = NA, maxYr = 2023)
#' #temp_all_years_monthly(climate_clean)
#' #temp_monthly_mean_historical(climate_clean, minYr = NA, maxYr = 2023)
#' #temp_count_NAs(climate_clean)
#' 
temperature_stats_daily <- function(climate_data, minYr = NA, maxYr = NA){

  # if numbers are left as NA then automatically populate years with minimum and maximum values
  if (is.na(minYr)){
    minYr <- min(climate_data$LOCAL_YEAR)
  }
  
  if (is.na(maxYr)){
    maxYr <- max(climate_data$LOCAL_YEAR)
  }

  # Temp stats
  # LOCAL MONTH | LOCAL DAY | MEAN | MEDIAN | RECORD_MAX| RECORD_LOW | Q25_MEAN | q75_MEAN | MAX_MEAN | MIN_MEAN
  temp_stats_daily <- climate_data %>%
    dplyr::filter(LOCAL_YEAR >= minYr, 
                  LOCAL_YEAR <= maxYr) %>%
    dplyr::group_by(STATION_NAME, LOCAL_MONTH, LOCAL_DAY) %>%
    dplyr::summarise(mean = mean(MEAN_TEMPERATURE, na.rm = TRUE),
                     median = median(MEAN_TEMPERATURE, na.rm = TRUE),
                     record_max = max(MAX_TEMPERATURE, na.rm = TRUE), 
                     record_low = min(MIN_TEMPERATURE, na.rm = TRUE), 
                     q25_mean = quantile(MEAN_TEMPERATURE, 0.25, na.rm = TRUE),
                     q75_mean = quantile(MEAN_TEMPERATURE, 0.75, na.rm = TRUE),
                     max_mean = max(MEAN_TEMPERATURE, na.rm = TRUE),
                     min_mean = min(MEAN_TEMPERATURE, na.rm = TRUE)) 

  return(temp_stats_daily)

}

#' @export
#' @rdname temperature_stats_daily
temp_all_years_monthly <- function(climate_data){
  
  # temperature monthly for all years
  # YEAR | MONTH | MONTHLY_MEAN | MONTHLY_MAX_MEAN | MONTHLY_MIN_MEAN
  temp_monthly <- climate_data %>%
    dplyr::group_by(STATION_NAME, LOCAL_YEAR, LOCAL_MONTH) %>%
    dplyr::summarise(monthly_mean = mean(MEAN_TEMPERATURE, na.rm = TRUE), 
                     monthly_max_mean = mean(MAX_TEMPERATURE, na.rm = TRUE),
                     monthly_min_mean = mean(MIN_TEMPERATURE, na.rm = TRUE))
                                             
  
  return(temp_monthly)
}

#' @export
#' @rdname temperature_stats_daily
temp_monthly_mean_historical <- function(climate_data, minYr = NA, maxYr = NA){
  
  # if numbers are left as NA then automatically populate years with minimum and maximum
  if (is.na(minYr)){
    minYr <- min(climate_data$LOCAL_YEAR)
  }
  
  if (is.na(maxYr)){
    maxYr <- max(climate_data$LOCAL_YEAR)
  }
  
  # temperature monthly for all years
  # MONTH | MONTHLY_MEAN | MONTHLY_MAX_MEAN | MONTHLY_MIN_MEAN
  temp_monthly <- climate_data %>%
    dplyr::filter(LOCAL_YEAR >= minYr, 
           LOCAL_YEAR <= maxYr) %>%
    dplyr::group_by(STATION_NAME, LOCAL_MONTH) %>%
    dplyr::summarise(monthly_mean = mean(MEAN_TEMPERATURE, na.rm = TRUE), 
                     monthly_max_mean = mean(MAX_TEMPERATURE, na.rm = TRUE),
                     monthly_min_mean = mean(MIN_TEMPERATURE, na.rm = TRUE))
  
  
  return(temp_monthly)
}

#' @export
#' @rdname temperature_stats_daily
temp_count_NAs <- function(climate_data){
  
  # Missing data per year     
  # YEAR | COUNT_NA |
  temp_count <- climate_data %>%
    dplyr::group_by(STATION_NAME, LOCAL_YEAR) %>%
    dplyr::summarise(count_na = sum(is.na(TOTAL_PRECIPITATION)))
  
  return(temp_count)
}

