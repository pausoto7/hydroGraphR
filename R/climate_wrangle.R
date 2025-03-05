#'  Downloads and wrangles hydro data
#'
#' @description 
#' `clean_climate` This wrangles *daily* climate data acquired from the Canadian  
#' climate extraction tool 
#' (https://climate-change.canada.ca/climate-data/#/daily-climate-data) and gets 
#' it ready for analysis. 
#' 
#' 
#' @param climate_raw A data frame containing raw data downloaded from the 
#' Canadian climate extraction tool
#'
#' @return A clean data frame that can be used in [precip_stats_daily()], [temperature_stats_daily()],
#' and [temp_plotting_historical()]
#' 
#' @importFrom dplyr select mutate group_by
#' @importFrom lubridate ymd_hms ymd
#' @importFrom utils head
#' @importFrom stats median quantile
#' @examples
#'# Read raw climate csv files and bind into one data frame
#'# Files should be "daily" climate data from climate extraction tool 
#' files <- get_example_file()
#' file_paths <- get_example_file(grep("merritt_", files, value = TRUE))
#' data_list <- lapply(file_paths, read.csv)
#' climate_raw <- do.call(rbind, data_list)
#' #clean_climate(climate_raw)
#'

clean_climate <- function(climate_raw){

  excluded_variables <- c("x", "y", "CLIMATE_IDENTIFIER", "ID", "PROVINCE_CODE", "MEAN_TEMPERATURE_FLAG", "MIN_TEMPERATURE_FLAG", "MAX_TEMPERATURE_FLAG", "TOTAL_PRECIPITATION_FLAG", 
                          "HEATING_DEGREE_DAYS", "COOLING_DEGREE_DAYS", "MIN_REL_HUMIDITY", "MAX_REL_HUMIDITY", "SPEED_MAX_GUST",
                          "TOTAL_RAIN_FLAG", "TOTAL_SNOW_FLAG", "SNOW_ON_GROUND_FLAG", "DIRECTION_MAX_GUST", "DIRECTION_MAX_GUST_FLAG", "SPEED_MAX_GUST_FLAG", 
                          "COOLING_DEGREE_DAYS_FLAG", "HEATING_DEGREE_DAYS_FLAG", "MIN_REL_HUMIDITY_FLAG", "MAX_REL_HUMIDITY_FLAG")
  
  # selects only not excluded variables. one_of is used in case of difference in columns from input data.
  climate_clean <- climate_raw %>% 
    dplyr::select(-dplyr::one_of(excluded_variables)) %>%
    dplyr::mutate(DateTime = ymd_hms(LOCAL_DATE))
  
  col_names_clean_climate <- colnames(climate_clean)
  
  numeric_columns <- c("LOCAL_YEAR", "LOCAL_DAY", "LOCAL_MONTH","TEMP", "MIN_TEMPERATURE", "MEAN_TEMPERATURE", "MAX_TEMPERATURE", "TOTAL_PRECIPITATION", 
                       "TOTAL_SNOW", "TOTAL_RAIN")
  
  #only select columns that exist to make numeric
  num_cols_exist <- numeric_columns[which(numeric_columns %in% col_names_clean_climate)]
  
  #change the appropriate columns into numeric
  climate_clean <- climate_clean %>%
    dplyr::mutate(across(all_of(num_cols_exist), as.numeric))
  
  stn_name <- stringr::str_replace_all(unique(climate_clean$STATION_NAME), " ", "_")

  return(climate_clean)
}
