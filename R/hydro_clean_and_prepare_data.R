#' Create a Comprehensive Dataframe of Locations and Date Ranges
#'
#' @description
#' `combine_csv_tidyhydat` merges discharge data downloaded from the WSC website 
#' with data obtained through the `tidyhydat` package, creating a single comprehensive 
#' dataframe. This function is particularly useful for ensuring that all relevant 
#' data, including manually downloaded datasets, are consolidated into one dataset 
#' for further analysis.
#' 
#' @param tidyhydat_hydro A dataframe containing hydrological data retrieved 
#' using the `tidyhydat` package.
#' @param csv_Q_dl A dataframe containing discharge data manually downloaded 
#' from the WSC website. 
#'
#' @return A single dataframe that combines both sources, including updated information 
#' on the relevant date ranges and start/end dates for the data.
#' @export
#' 
#' @examples
#' # Example 1: Download and combine data
#' 
#' # Step 1: Retrieve data using tidyhydat
#' tidy_data <- dl_hydro(station_number = "08HB034", nickname = "Nanaimo River")
#' 
#' # Step 2: Prepare WSC data
#' file_path <- get_example_file("Nanaimo_08HB034_QR_20240104T2154.csv")
#' wsc_data <- reformat_Q_dl_daily(file_path, "Nanaimo River")
#' 
#' # Step 3: Combine datasets
#' combined_data <- combine_csv_tidyhydat(tidy_data, wsc_data)
#'
#' @details
#' This function ensures no data gaps by integrating manually downloaded datasets 
#' with those retrieved programmatically from `tidyhydat`. Any discrepancies in 
#' date ranges between the two sources are resolved to provide a complete and 
#' accurate dataset.

combine_csv_tidyhydat <- function(tidyhydat_hydro, csv_Q_dl){

  #bind layers
  all_hydro_sites_combined <- rbind(tidyhydat_hydro, csv_Q_dl)
  
  #get start/end date
  all_hydro_sites_combined_unique <- all_hydro_sites_combined %>%
    dplyr::select(-c(Date, Value)) %>%
    dplyr::distinct()
  
  # get location list
  loc_list <- unique(all_hydro_sites_combined_unique$STATION_NUMBER)  
  
  #empty list for use in below for loop
  df_list <- list()
  
  for (stations in 1:length(loc_list)){
    
    all_hydro_sites_1Loc <- all_hydro_sites_combined_unique %>%
      dplyr::filter(STATION_NUMBER == loc_list[stations])
    
    startD <- min(all_hydro_sites_1Loc$start_date)
    endD <- max(all_hydro_sites_1Loc$end_date)
    
    u_years <- list(unlist(all_hydro_sites_1Loc$unique_yrs))
    
    unique_yrs_df <- data.frame(STATION_NUMBER = loc_list[stations], 
                                unique_yrs = NA,
                                start_date = startD, 
                                end_date = endD
    )
    unique_yrs_df$unique_yrs <- u_years
    
    df_list[[stations]] <- unique_yrs_df
    
  }  
  
  all_hydro_sites_combined_clean <- all_hydro_sites_combined %>%
    dplyr::select(Date, Value, dplyr::any_of(c("NickName")), STATION_NUMBER)
  
  
  all_hydro_sites_ready <- do.call(rbind, df_list) %>%
    dplyr::full_join(all_hydro_sites_combined_clean)
  
  
  return(all_hydro_sites_ready)
  
}


