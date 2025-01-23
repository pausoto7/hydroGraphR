#' Wrangles temperature data and produces a plot with historical and YOI 
#' temperature data.
#'
#' @description
#' `temp_plotting_historical` produces a historical temperature plot with a 
#' singular year of interest (YOI) highlighted if desired.
#' 
#' @param climate_data a dataframe with the climate information. 
#' @param minYr The minimum year included in the historical plot calculations. 
#' Leave as NA if you want the minimum year in historical data.
#' @param maxYr The maximum year included in the historical plot calculations
#' Leave as NA if you want the maximum year present in historical data. Also 
#' consider what your YOI is and if you want that included or not.
#' @param YOI A single year of interest that will be compared with a 
#' ribbon of historical data.
#' 
#' @importFrom tidyr pivot_longer
#'
#' @return A plot with historical temperature stats and a YOI line if indicated.

#' @examples
#' # First, clean raw climate data using clean_climate()
#' 
temp_plotting_historical <- function(climate_data, minYr = NA, maxYr = NA, YOI = NA){
  
  temp_historical_daily <- temperature_stats_daily(climate_data, minYr, maxYr)
  
  climate_data_YOI <- climate_data %>%
    dplyr::filter(LOCAL_YEAR == YOI) %>% 
    dplyr::select(LOCAL_DATE, MEAN_TEMPERATURE, MAX_TEMPERATURE, MIN_TEMPERATURE) 
  
  climate_data_YOI_longer <- climate_data_YOI %>%
    tidyr::pivot_longer(names_to = "TEMP_STATS_TYPE", cols = c(MEAN_TEMPERATURE, MAX_TEMPERATURE, MIN_TEMPERATURE), values_to = "VALUE")
  
  # produce plot without YOI included
  if (is.na(YOI)){
    
    temp_annual_plotting <- temp_historical_daily %>%
      dplyr::mutate(arbitrary_date = sprintf("2020-%s-15", LOCAL_MONTH)) %>%
      dplyr::mutate(arbitrary_date = as.Date(arbitrary_date))
    
    historical_temp_plot <- ggplot() +
      geom_ribbon(temp_historical_daily, mapping = aes(x = arbitrary_date, ymin = record_low, ymax = record_max), fill = "lightblue", alpha = 0.5) +
      geom_ribbon(temp_historical_daily, mapping = aes(x = arbitrary_date, ymin = q25_mean, ymax = q75_mean), fill = "skyblue", alpha = 0.6) +
      geom_line(temp_historical_daily,mapping = aes( x = arbitrary_date, y = mean), linewidth = 1, color = "royalblue") +
      theme_bw() +
      scale_y_continuous(name ="Temperature (\u00B0C)" ) +
      scale_x_date(name = "Date", breaks = "1 month", date_labels ="%b")
    
    locs <- stringr::str_replace(unique(temp_historical_daily$STATION_NAME), " ", "_")
    loc_string <- stringr::str_flatten(locs, collapse = "_")
    
    file_path <- sprintf("figures/%s_temp_plot.jpeg", loc_string)
    
    #produce plot with YOI included 
  }else{
    
    temp_historical_daily <- temp_historical_daily %>%
      dplyr::mutate(arbitrary_date = ymd(paste(YOI, LOCAL_MONTH, LOCAL_DAY, sep = "-")))
  
    historical_temp_plot <- ggplot() +
     geom_ribbon(temp_historical_daily, mapping = aes(x = arbitrary_date, ymin = record_low, ymax = record_max), fill = "lightblue", alpha = 0.5) +
     geom_ribbon(temp_historical_daily, mapping = aes(x = arbitrary_date, ymin = q25_mean, ymax = q75_mean), fill = "skyblue", alpha = 0.6) +
      geom_line(temp_historical_daily,mapping = aes( x = arbitrary_date, y = mean), linewidth = 1, color = "royalblue") +
      geom_linerange(climate_data_YOI, mapping = aes(x = as.Date(LOCAL_DATE), ymin = MIN_TEMPERATURE, ymax = MAX_TEMPERATURE), 
                     color = "violetred", linewidth =1.5, alpha = 0.7 ) +
      geom_line(climate_data_YOI,  mapping = aes(x = as.Date(LOCAL_DATE),  y = MEAN_TEMPERATURE)) +
      theme_bw() +
      scale_y_continuous(name ="Temperature (\u00B0C)" ) +
      scale_x_date(name = "Date", breaks = "1 month", date_labels ="%b")
    
    locs <- stringr::str_replace(unique(temp_historical_daily$STATION_NAME), " ", "_")
    loc_string <- stringr::str_flatten(locs, collapse = "_")
    
    file_path <- sprintf("figures/%s_%i_temp_plot.jpeg", loc_string, YOI)
    
  }
  
  ggsave(file_path, plot = historical_temp_plot, width = 9, height = 5.5, create.dir = T)
  simpleMessage(sprintf("Figure saved to %s", file_path))
  # return(historical_temp_plot)
} 