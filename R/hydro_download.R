#' Download and Reformat Hydrology Data
#'
#' @description
#' - **`dl_hydro`**: Downloads hydrology data for specified station(s) using the `tidyhydat` 
#'   package, optionally appends a nickname to each station, and outputs the data in long format.
#'
#' - **`reformat_Q_dl_daily`**: Reformats manually downloaded daily hydrology data 
#'   (from the WSC website) into the same format as data downloaded through `tidyhydat`, 
#'   ensuring consistency for analysis.
#'
#' @param station_number A single station number or a list of station numbers to download data for.
#' @param nick_name A single nickname or a list of nicknames corresponding to `station_number`. 
#'   Must match the length and order of `station_number`. Use `NA` if no nicknames are required.
#' @param Q_csv_path A character string specifying the path to a CSV file containing daily 
#'   hydrology data manually downloaded from the WSC website.
#'
#' @return A dataframe containing hydrologic data for all specified stations in long format.
#'
#' @examples
#' # Example 1: Download data for a single station
#' dl_hydro(station_number = "08LD001", nick_name = "Adams River")
#'
#' # Example 2: Download data for multiple stations
#' dl_hydro(station_number = c("08LD001", "08LC002"), 
#'          nick_name = c("Adams River", "Shuswap River"))
#'
#' # Example 3: Reformat manually downloaded data
#' file_path <- get_example_file("Nanaimo_08HB034_QR_20240104T2154.csv")
#' reformat_Q_dl_daily(file_path)

#' @export
dl_hydro <- function(station_number, nick_name = NULL){
  
  # QC inputs
  if(is.null(nick_name)){
    #No nicknames will be added"
    
  }else if (!is.null(nick_name) & length(station_number) != length(nick_name)){st
    stop(simpleError("station number and location name are of different lengths. 
                     Please enter matching station numbers and names into input"))
    
  }else if(length(station_number) == 0){
    stop(simpleError("station_number was left blank. Please fill in properly and try again."))
    
  }else{#station_number and nick_name number match"
    
    # check that none of the items are NA or empty
    if (!all(sapply(nick_name, function(x) is.character(x) && !is.na(x) && x != ""))) {
      stop("Some variables are not valid. Check for non-character, NA, or empty strings.")      
    }
    
  }

  # start code to download and clean downloaded hydro sites
  hydro_site_list <- list()
  
  for(hydro_locations in  1:length(station_number)){
    
    # download and wrangle historical daily discharge ------------------------------------------------
    
    # Attempt to retrieve flow data
    hydro_daily_flows <- tryCatch({
      tidyhydat::hy_daily_flows(
        station_number = station_number[hydro_locations])
      
    }, error = function(e) {
      # Handle the error by returning NULL or a custom message
      message(sprintf("No historical flow data for station %s in HYDAT", station_number[hydro_locations]))
      NULL
    })
    
    has_daily_flow <- !is.null(hydro_daily_flows) && nrow(hydro_daily_flows) > 0
    
    if (has_daily_flow){
      # Historical daily flows present
    }else{
      hydro_daily_flows <- data.frame(
        STATION_NUMBER = character(), 
        Date = as.Date(character()), 
        Value = numeric(), 
        Parameter = as.character(), 
        Symbol = as.character(),
        stringsAsFactors = FALSE
      )
    }
    
    q_hist_final_date <- utils::tail(hydro_daily_flows, 1)$Date
    
    real_time_start_date <- ifelse(is.na(q_hist_final_date) | length(q_hist_final_date) == 0, Sys.Date()-540, q_hist_final_date )
    
    
    # download and wrangle realtime discharge ------------------------------------------------

    # Attempt to retrieve flow data
    flow_real_time <- tryCatch({
      tidyhydat::realtime_ws(
        station_number = station_number[hydro_locations], 
        parameters = 47, 
        start_date = as.Date(real_time_start_date), 
        end_date = Sys.Date()
      )
      
    }, error = function(e) {
      # Handle the error by returning NULL or a custom message
      message(sprintf("No realtime flow data for station %s in HYDAT ", station_number[hydro_locations]))
      NULL
    })
    
    
    if (!is.null(flow_real_time)){
      
      # Ensure that output df is actually only flow data.Temp or level data may sometimes be put in its place if there is no flow data present. 
      flow_real_time <- flow_real_time %>% 
        dplyr::filter(Parameter == 47)
      
    }

    
    # Check if flow data is present
    has_realtime_flow <- !is.null(flow_real_time) && nrow(flow_real_time) > 0
    
    if (has_realtime_flow) {
      
      flow_real_time <- flow_real_time %>% 
        dplyr::select(-Name_En, -Unit, -Grade, -Approval, -Code) %>%
        dplyr::group_by(Date = as.Date(Date)) %>%
        dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>%
        dplyr::mutate(Parameter = "Flow", 
                      STATION_NUMBER = station_number[hydro_locations]) 
      
    } else {
      flow_real_time <-  data.frame(
        STATION_NUMBER = character(), 
        Date = as.Date(character()), 
        Value = numeric(), 
        Parameter = as.character(), 
        Symbol = as.character(),
        stringsAsFactors = FALSE
      )
    }
    
    
    
    # download and wrangle historical water level -------------------------------------------------------
    
    hydro_daily_WL<- tryCatch({
      # Attempt to retrieve level data
      tidyhydat::hy_daily_levels(
        station_number = station_number[hydro_locations])
      
    }, error = function(e) {
      # Handle the error by returning NULL or a custom message
      message(sprintf("No historical water level data for station %s in HYDAT ", station_number[hydro_locations]))
      
      NULL
    })
    
    has_daily_WL <- !is.null(hydro_daily_WL) && nrow(hydro_daily_WL) > 0
    
    if (has_daily_WL){
      # There is daily flows")
    }else{
      hydro_daily_WL <-  data.frame(
        STATION_NUMBER = character(), 
        Date = as.Date(character()), 
        Value = numeric(), 
        Parameter = as.character(), 
        unique_yrs = integer(), 
        #start_date = as.Date(character()), 
        #end_date = as.Date(character()),
        stringsAsFactors = FALSE
      )
    }
    

    WL_hist_final_date <- utils::tail(hydro_daily_flows, 1)$Date
    
    real_time_start_date <- ifelse(is.na(WL_hist_final_date) || length(WL_hist_final_date) == 0, 
                                   Sys.Date()- lubridate::days(540) , WL_hist_final_date )

    
    
    
    # download and wrangle realtime water level -------------------------------------------------------

    # Attempt to retrieve WL data
    WL_real_time <- tryCatch({
      tidyhydat::realtime_ws(
        station_number = station_number[hydro_locations], 
        parameters = 46, 
        start_date = as.Date(real_time_start_date), 
        end_date = Sys.Date()
      )
      
    }, error = function(e) {
      # Handle the error by returning NULL
      NULL
    })
    
    if (!is.null(WL_real_time)){
      
      # Ensure that output df is actually only level data. 
      # Temp or Q data may sometimes be put in its place if there is no flow data present. 
      WL_real_time <- WL_real_time %>% 
        dplyr::filter(Parameter == 46)
      
    }
    

    # Check if WL data is present
    has_realtime_WL <- !is.null(WL_real_time) && nrow(WL_real_time) > 0
    
    if (has_realtime_WL) {
      
      WL_real_time <- WL_real_time %>% 
        dplyr::select(-Name_En, -Unit, -Grade, -Approval, -Code) %>%
        dplyr::group_by(Date = as.Date(Date)) %>%
        dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>%
        dplyr::mutate(Parameter = "Level", 
                      STATION_NUMBER = station_number[hydro_locations]) 
      
    } else {
      WL_real_time <-  data.frame(
        STATION_NUMBER = character(), 
        Date = as.Date(character()), 
        Value = numeric(), 
        Parameter = as.character(), 
        Symbol = as.character(),
        stringsAsFactors = FALSE
      )
      message(sprintf("No realtime water level data for station %s in HYDAT ", station_number[hydro_locations]))
      
    }
    
    
    
    # join flow and level data ----------------------------------------------
    
    all_Q_data <- hydro_daily_flows %>%
      dplyr::full_join(flow_real_time)
    
    all_WL_data <- hydro_daily_WL %>%
      dplyr::full_join(WL_real_time)
    
    # all_q_data years --------------------------------------------------------------
    if (nrow(all_Q_data)> 0){
      
      #get list of unique years for date ranges function
      unique_years_list_Q <- unique(lubridate::year(all_Q_data$Date))
      all_Q_data$unique_yrs <- list(unique_years_list_Q)
      
    }else{
      
      all_Q_data$unique_yrs <- list()
    }
    
    # all_wl_data years --------------------------------------------------------------
    
    if (nrow(all_WL_data) > 0){
      
      #get list of unique years for date ranges function
      unique_years_list_WL <- unique(lubridate::year(all_WL_data$Date))
      all_WL_data$unique_yrs <- list(unique_years_list_WL)
      
    }else{
      
      all_WL_data$unique_yrs <- list()
      
    }
    
    # join ------------------------------------------------
    
    all_hydro_data <- all_Q_data %>%
      dplyr::full_join(all_WL_data)
    
    
    # If location is not NA then add nickname column
    if (!is.null(nick_name)){
      all_hydro_data <- dplyr::mutate(all_hydro_data, NickName = nick_name[hydro_locations])
    }
    
    hydro_site_list[[hydro_locations]] <- all_hydro_data
    
  }
  
  #bind all list elements
  all_hydro_sites <- do.call(rbind, hydro_site_list)
  
  if ("nick_name" %in% names(all_hydro_sites)) {
    all_hydro_sites <- all_hydro_sites %>%
      dplyr::select(STATION_NUMBER, nick_name, Date, Value, Parameter, unique_yrs)
    
  } else {
    all_hydro_sites <- all_hydro_sites %>%
      dplyr::select(STATION_NUMBER, Date, Value,  Parameter, unique_yrs)    
    
  }
  
  all_hydro_sites <- all_hydro_sites %>%
    dplyr::group_by(STATION_NUMBER) %>%
    dplyr::mutate(start_date = min(Date), 
                  end_date = max(Date))
  
  # save dataframe
  return(all_hydro_sites)
  #("Function download_save succesfull")
  
  
}




#' @export
#' @rdname dl_hydro
reformat_Q_dl_daily <- function(Q_csv_path, nick_name = NULL){

  # read 3rd line of csv which contains station number which is then pulled out
  csv_data_station_num_raw <- utils::read.csv(Q_csv_path, skip = 3, header = FALSE)
  csv_data_station_num <- csv_data_station_num_raw[1,1]
  string_length <- nchar(csv_data_station_num)
  csv_data_station_num <- substr(csv_data_station_num, string_length-12, string_length-6)
  
  #read csv
  csv_data_raw <- utils::read.csv(Q_csv_path, skip = 9)
  
  #give cols better names
  colnames(csv_data_raw) <- c("DateTime", "Parameter", "Value", "Approval", "Qualifier")
  
  #clean up and set stats daily
  Q_data <- csv_data_raw %>%
    dplyr::group_by(Date = as.Date(DateTime)) %>%
    dplyr::reframe(Value = mean(Value)) %>%
    dplyr::select(Date, Value) %>%
    dplyr::mutate(start_date = min(Date), 
                  end_date = max(Date), 
                  STATION_NUMBER = csv_data_station_num) 
  
  station_number <- csv_data_station_num
  
  
  
  # QC inputs
  if(is.null(nick_name)){
    #No nicknames will be added"
    
  }else if (!is.null(nick_name) & length(station_number) != length(nick_name)){
    stop(simpleError("station number and location name are of different lengths. 
                     Please enter matching station numbers and names into input"))
    
  }else if(length(station_number) == 0){
    stop(simpleError("station_number was left blank. Please fill in properly and try again."))
    
  }else{#station_number and nick_name number match"
    
    # check that none of the items are NA or empty
    if (!all(sapply(nick_name, function(x) is.character(x) && !is.na(x) && x != ""))) {
      stop("Some variables are not valid. Check for non-character, NA, or empty strings.")      
    }
    
  }
  
  # add list of unique years for later use for date_ranges function
  unique_years_list <- unique(lubridate::year(Q_data$Date))
  Q_data$unique_yrs <- list(unique_years_list)
  
  return(Q_data)
}
