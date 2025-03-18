#' Create Hydrology Summary Statistics
#'
#' @description
#' The following functions summarize hydrological statistics:
#'
#' - **`create_hydro_stats_historical`**: Summarizes historical hydrological statistics 
#'   into a dataframe, grouped by month and day.
#'
#' - **`create_hydro_stats_singleYr`**: Generates statistical summaries for a single 
#'   year of data. This function filters a specific year from the historical data and 
#'   produces corresponding statistics.
#'
#' - **`create_hydro_stats_from_Q_dl`**: Processes manually downloaded discharge data 
#'   (from the WSC website) to generate statistics for a single year. This may be
#'   useful if recent data is not yet available in `tidyhydat`.
#'
#' **Note**: All data from February 29 has been excluded from calculations to address 
#' complications related to leap years. This avoids statistical biases caused by only 
#' a subset (1/4) of years including February 29 in the dataset.
#'
#' @param hydro_data A dataframe containing hydrology data in the format output by `dl_hydro`.
#' @param date_minimum A character string specifying the earliest date for the summary, 
#' in "yyyy-mm-dd" format. Defaults to the earliest date in the dataset.
#' @param date_maximum A character string specifying the latest date for the summary, 
#' in "yyyy-mm-dd" format. Defaults to the latest date in the dataset.
#' @param YOI An integer specifying a single year for statistical summarization. 
#' This is used to create the YOI trace for the hydrograph.
#' @param WY A logical value indicating whether to filter data by water year 
#' (`TRUE`) or calendar year (`FALSE`).
#' @param Q_csv_path A character string specifying the file path to a CSV containing 
#' manually downloaded discharge data from the WSC. The dataset should represent a 
#' single year of data for hydrograph generation.
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @return A dataframe containing hydrological statistics summarized by station and day.
#' @export
#'
#' @examples
#' # Example 1: Historical statistics
#' hydro_data <- dl_hydro(station_number = "08LD001", nickname = "Adams River")
#' 
#' # Generate historical statistics
#' create_hydro_stats_historical(hydro_data)
#' 
#' # Specify date range for summary
#' create_hydro_stats_historical(hydro_data, date_minimum = "2015-11-02", 
#'                                date_maximum = "2021-12-31")
#'
#' # Example 2: Single year statistics
#' create_hydro_stats_singleYr(hydro_data, YOI = 2021, WY = FALSE)
#'

create_hydro_stats_historical <- function(hydro_data, date_minimum = NA, date_maximum = NA){
  
  if (is.na(date_minimum)){
    date_minimum <- min(hydro_data$Date)
  }else{
    date_minimum <- lubridate::ymd(date_minimum)
    
  }
  
  if (is.na(date_maximum)){
    date_maximum <- max(hydro_data$Date)
  }else{
    date_maximum <- lubridate::ymd(date_maximum)
  }
  
  
  if ("nickname" %in% colnames(hydro_data)) {
    start_end_dates <- hydro_data %>%
      dplyr::select(STATION_NUMBER, NickName, start_date, end_date) %>%
      dplyr::distinct()
  
    } else {
      start_end_dates <- hydro_data %>%
        dplyr::select(STATION_NUMBER, start_date, end_date) %>%
        dplyr::distinct()
    }
  

  # Create a sequence of all dates for the year 2023
  all_dates <- tibble::tibble(Date = seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day"))
  
  # Create a sequence of all dates for the water year (starting from October 1st of the previous year)
  all_dates_WY <- tibble::tibble(Date = seq.Date(from = as.Date("2022-10-01"), to = as.Date("2023-09-30"), by = "day"))
  

  # get historical stats
  hydro_data_historical_all <- hydro_data %>% 
    dplyr::mutate(
      year_col = lubridate::year(Date),
      month_col = lubridate::month(Date),
      day_col = lubridate::day(Date)
    ) %>%
    dplyr::filter(!(month_col == 2 & day_col == 29)) %>% # Exclude Feb 29 b/c will result in error 
    dplyr::filter(Date < date_maximum, Date > date_minimum) %>%
    dplyr::group_by(Parameter, STATION_NUMBER, month_col, day_col) %>%
    dplyr::summarise(
      # try catch accounts for Value columns where all values are NULL/NA and stat gives an error
      mean = tryCatch(mean(Value, na.rm = TRUE), error = function(e) NA),
      median = tryCatch(stats::median(Value, na.rm = TRUE), error = function(e) NA),
      max = tryCatch(ifelse(all(is.na(Value)), NA, max(Value, na.rm = TRUE)), error = function(e) NA),
      min = tryCatch(ifelse(all(is.na(Value)), NA, min(Value, na.rm = TRUE)), error = function(e) NA),
      q5 = tryCatch(stats::quantile(Value, 0.05, na.rm = TRUE), error = function(e) NA),
      q25 = tryCatch(stats::quantile(Value, 0.25, na.rm = TRUE), error = function(e) NA),
      q75 = tryCatch(stats::quantile(Value, 0.75, na.rm = TRUE), error = function(e) NA),
      q95 = tryCatch(stats::quantile(Value, 0.95, na.rm = TRUE), error = function(e) NA)
    ) %>%
    dplyr::mutate(
      arbitrary_date = tryCatch(lubridate::ymd(paste("2023", month_col, day_col, sep = "-")), error = function(e) NA),
      arbitrary_date_WY = tryCatch(
        lubridate::ymd(paste(ifelse(month_col >= 10, 2023 - 1, 2023), month_col, day_col, sep = "-")),
        error = function(e) NA
      )
    )

  # get date ranges
  ranges_df <- get_date_ranges(hydro_data)
  
  # join historical, start/end dates, and date ranges df's
  hydro_data_historical <- hydro_data_historical_all %>%
    dplyr::full_join(start_end_dates, relationship = "many-to-many") %>%
    dplyr::full_join(ranges_df)

  return(hydro_data_historical)
  
}

#' @export
#' @rdname create_hydro_stats_historical
create_hydro_stats_singleYr <- function(hydro_data, YOI, WY = FALSE){
  
  full_dates <- data.frame(Date = seq.Date(min(hydro_data$Date), max(hydro_data$Date), by = "day"))

  # Create a sequence of all dates for the year 2023
  all_dates <- tibble::tibble(Date = seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day"))
  
  # Create a sequence of all dates for the water year (starting from October 1st of the previous year)
  all_dates_WY <- tibble::tibble(Date = seq.Date(from = as.Date("2022-10-01"), to = as.Date("2023-09-30"), by = "day"))
  
  hydro_data <- hydro_data %>%
    dplyr::mutate(year_col = lubridate::year(Date), 
                  month_col = lubridate::month(Date),
                  day_col = lubridate::day(Date),
                  WY_col = ifelse(month_col >= 10, year_col + 1, year_col)) %>%
    dplyr::filter(!(month_col == 2 & day_col == 29)) %>% # Exclude Feb 29 b/c will result in error in historical
    dplyr::mutate(arbitrary_date = lubridate::ymd(paste("2023", month_col, day_col, sep = "-")),
                  arbitrary_date_WY = lubridate::ymd(paste(ifelse(month_col >= 10, 2023 - 1, 2023), month_col, day_col, sep = "-"))) %>%
    dplyr::right_join(all_dates, by = c("arbitrary_date" = "Date")) %>%
    dplyr::right_join(all_dates_WY, by = c("arbitrary_date_WY" = "Date")) %>%
    dplyr::arrange(arbitrary_date)
  
  
  if (is.logical(WY)){
    # Filter by water year or calendar year based on WY input
    if (WY) {
      
      hydro_data_1yr <- hydro_data %>%
        dplyr::filter(WY_col == YOI)
      
    } else{
      
      hydro_data_1yr <- hydro_data %>%
        dplyr::filter(year_col == YOI)
    }
    
  }else{
    stop("Please enter a valid logical value for WY")
  }

  
  return(hydro_data_1yr)
  
}



#' @export
#' @rdname create_hydro_stats_historical
create_hydro_stats_from_Q_dl <- function(Q_csv_path){

  csv_data_raw <- utils::read.csv(Q_csv_path, skip = 9)
  
  Q_data <- csv_data_raw %>%
    dplyr::rename(DateTime = Date..PST., 
           flow = Value.m..s.) %>%
    dplyr::select(DateTime, flow) 

  Q_data_summarized <- Q_data %>%
    dplyr::mutate(year_col = lubridate::year(DateTime), 
                  month_col = lubridate::month(DateTime),
                  day_col = lubridate::day(DateTime)) %>%
    dplyr::filter(!(month_col == 2 & day_col == 29)) %>% # Exclude Feb 29
    dplyr::group_by(year_col, month_col, day_col) %>%
    dplyr::summarise(mean = mean(flow)) %>% 
    dplyr::mutate(arbitrary_date = lubridate::ymd(paste("2023", month_col, day_col, sep = "-")),
                    arbitrary_date_WY = lubridate::ymd(paste(ifelse(month_col >=10, 2023+1, 2023), month_col, day_col, sep = "-")))
  
  return(Q_data_summarized)
}

