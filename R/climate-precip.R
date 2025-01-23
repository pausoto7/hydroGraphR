#'  Downloads and wrangles hydro data
#'
#' @description 
#' 
#' `precip_stats_daily` This function re-formats data that was manually downloaded 
#'  from the canadian climate extraction tool.
#'  Output: LOCAL MONTH | LOCAL DAY | MEAN | MEDIAN | Q25 | q75 | MAX | MIN | ARB DATE
#' `precip_all_years_monthly` This function reformats data into  monthly precipitation
#' for all  years using data that that was manually downloaded from the canadian climate extraction tool.
#'  Output: YEAR | MONTH | MONTHLY_SUM
#' `precip_averaged_monthly` This function reformats data into average historical monthly
#'  precipitaiton 
#'  MONTH | MONTHLY AVERAGE | MAX | MIN
#' `precip_count_NAs` This function counts annual NA's in a dataset
#' Output:  YEAR | COUNT_NA 
#' `total_annual_precip` Sums total annual precipitation
#' `max_min_ave_annual_precip` Maximum and minimum historical annual precipitation
#' 
#' 
#' @param climate_data climate data from climate station 
#' @param YOI Year of Interest - Year that will be compared to historical stats.
#' @param average_annual_precipitation Dataframe output from [total_annual_precip()] 
#'
#' @return A dataframe precipitation statistics.

#' @examples
#' # First, clean raw climate data using clean_climate()
#' #climate_clean <- clean_climate(climate_raw)
#' 
#' # Calculate precipitation stats
#' #precip_stats_daily(climate_clean, YOI = 2023)
#' #precip_all_years_monthly(climate_clean)
#' #precip_averaged_monthly(climate_clean)
#' #precip_count_NAs(climate_clean)
#' 
#' #average_annual_precipitation <- total_annual_precip(climate_clean)
#' #max_min_ave_annual_precip(average_annual_precipitation)
#' 

precip_stats_daily <- function(climate_data, YOI = 2023){
  
  #YOI <- 2023

  # precipitation stats 
  # 
  precipitation_stats_daily <- climate_data %>% 
    dplyr::filter(LOCAL_YEAR != YOI) %>% 
    dplyr::group_by(STATION_NAME, LOCAL_MONTH, LOCAL_DAY) %>%
    dplyr::summarise(mean = mean(TOTAL_PRECIPITATION, na.rm = TRUE), 
              median = median(TOTAL_PRECIPITATION, na.rm = TRUE),
              q25 = quantile(TOTAL_PRECIPITATION, 0.25, na.rm = TRUE),
              q75 = quantile(TOTAL_PRECIPITATION, 0.75, na.rm = TRUE), 
              max = max(TOTAL_PRECIPITATION, na.rm = TRUE), 
              min = min(TOTAL_PRECIPITATION, na.rm = TRUE)) %>%
    dplyr::mutate(arbitrary_date = ymd(paste(YOI, LOCAL_MONTH, LOCAL_DAY, sep = "-")))
  
  return(precipitation_stats_daily)
  
}

#' @export
#' @rdname precip_stats_daily
precip_all_years_monthly <- function(climate_data){
  
  # Precipitation monthly for all years
  # YEAR | MONTH | MONTHLY_SUM
  precipitation_monthly <- climate_data %>%
    dplyr::group_by(STATION_NAME, LOCAL_YEAR, LOCAL_MONTH) %>%
    dplyr::summarise(monthly_sum = sum(TOTAL_PRECIPITATION, na.rm = TRUE))
  
  return(precipitation_monthly)
}

#' @export
#' @rdname precip_stats_daily
precip_averaged_monthly <- function(climate_data){
  
  precipitation_monthly <- precip_all_years_monthly(climate_data)
  
  # Stats for AVERAGE monthly precipitation 
  # MONTH | MONTHLY AVERAGE | MAX | MIN
  ave_monthly_precip <- precipitation_monthly %>%
    dplyr::group_by(STATION_NAME, LOCAL_MONTH) %>%
    dplyr::summarise(monthly_ave = mean(monthly_sum), 
              max = max(monthly_sum), 
              min = min(monthly_sum))

  return(ave_monthly_precip)
}

#' @export
#' @rdname precip_stats_daily
precip_count_NAs <- function(climate_data){
  
  # Missing data per year     
  # YEAR | COUNT_NA |
  precipitation_count <- climate_data %>%
    dplyr::group_by(STATION_NAME, LOCAL_YEAR) %>%
    dplyr::summarise(count_na = sum(is.na(TOTAL_PRECIPITATION)))
  
  return(precipitation_count)
}

#' @export
#' @rdname precip_stats_daily
total_annual_precip <- function(climate_data, YOI = 2023){

  # SUM TOTAL PRECIPITATION ANNUALLY
  average_annual_precipitation <- climate_data %>%
    dplyr::group_by(STATION_NAME, LOCAL_YEAR) %>%
    dplyr::summarise(annual_sum = sum(TOTAL_PRECIPITATION))
  
  return(average_annual_precipitation)
}

#' @export
#' @rdname precip_stats_daily
max_min_ave_annual_precip <- function(average_annual_precipitation){
  
  precip_ave_annual_stats <- average_annual_precipitation %>%
    dplyr::group_by(STATION_NAME) %>%
    dplyr::summarise(min_precip = min(annual_sum, na.rm = TRUE), 
              max_precip = max(annual_sum, na.rm = TRUE))
  
  return(precip_ave_annual_stats)
  
}
