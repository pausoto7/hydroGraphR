#' Create Hydrograph Plots
#'
#' @description
#' - **`create_hydrograph_separate`**: Generates separate hydrograph plots for specified locations.
#' - **`create_hydrograph_faceted`**: Creates a joint hydrograph plot for specified locations using `facet_grid`.
#'
#' @import ggplot2
#' @param all_hydro_sites_historical A data frame containing historical hydrology data.
#' @param all_hydro_sites_1yr A data frame containing one-year hydrology data.
#' @param parameter A character string specifying the parameter to plot. Options are `"flow"` or `"level"`.
#' @param output_type A character string indicating the output format. Options are `"JPEG"` for JPEG figure output 
#'   or `"print"` for HTML/Markdown output.
#' @param WY Logical value indicating whether to present hydrograph by water year (`TRUE`) or calendar year (`FALSE`).
#' @param fixed_y_scales A character string specifying whether the y-axis scale is `"fixed"` or `"free"` 
#'   across facets. Defaults to `"fixed"`.
#' @param custom_ymax_input A numeric value for a custom maximum y-axis value.
#' @param custom_ymin_input A numeric value for a custom minimum y-axis value.
#' @param jpeg_width A numeric value specifying the width of the figure (in pixels) for JPEG output.
#' @param jpeg_height A numeric value specifying the height of the figure (in pixels) for JPEG output.
#'
#' @return Hydrograph plots saved to the `"figures/"` folder.
#'
#' @examples
#' # Download hydrology data for multiple stations
#' hydro_data <- dl_hydro(station_number = c("08LD001", "08LC002"))
#'
#' # Generate historical and single-year hydrology statistics
#' all_hydro_sites_hist <- create_hydro_stats_historical(hydro_data)
#' all_hydro_sites_1yr <- create_hydro_stats_singleYr(hydro_data, YOI = 2021, WY = TRUE)
#'
#' # Create separate hydrograph plots
#' create_hydrograph_separate(all_hydro_sites_hist, all_hydro_sites_1yr, 
#'                            parameter = "flow", WY = TRUE)
#'
#' # Create faceted hydrograph plot
#' create_hydrograph_faceted(all_hydro_sites_hist, all_hydro_sites_1yr)
#' 
#' @export
create_hydrograph_separate <- function(all_hydro_sites_historical, all_hydro_sites_1yr, parameter, 
                                       output_type = "print", WY = FALSE,
                                        custom_ymax_input = NA, custom_ymin_input = NA){
  
  locations <- unique(all_hydro_sites_historical$STATION_NUMBER)

  # filter df's for parameter wanted
  param_info <- select_hydro_parameter(all_hydro_sites_historical, all_hydro_sites_1yr, parameter)
  
  #pull output values from function  
  all_hydro_sites_historical <- param_info[[1]]
  all_hydro_sites_1yr <- param_info[[2]]
  y_lab <- param_info[[3]]
  
  
  if(is.logical(WY)){
    # naming for plot label at bottom
    WY_label <- if(WY){
      "WY"
    }else{
      "calendar"
    }
    
  }else{
    stop("Please enter a valid logical value for WY")
  }

  
  
  for (location_num in 1:length(locations)){
    message("Processing station: ", locations[location_num], "\n")
    
    # run choose_hydro_timeline function. Suppress empty df warning because it's not a problem. 
    timeline_metadata <- withCallingHandlers(
      choose_hydro_timeline(all_hydro_sites_historical, all_hydro_sites_1yr, WY, locations, location_num),
      warning = function(w) {
        if (grepl("all_hydro_sites_1yr is empty. Historical values cannot be compared with this year", w$message)) {
          invokeRestart("muffleWarning")  # This suppresses only this specific warning
        }
      }
    )
    
    # pull values from object
    all_hydro_sites_historical_filtered <- timeline_metadata[[1]]
    all_hydro_sites_1yr_filtered <- timeline_metadata[[2]]
    year_label <- timeline_metadata[[3]]
    
    # calculate limits of x/y axis
    custom_ymax <- set_custom_limit(custom_ymax_input, all_hydro_sites_historical_filtered$q95, 
                                    all_hydro_sites_1yr_filtered$Value, multiplier = 1.04, direction = "max")
    
    custom_ymin <- set_custom_limit(custom_ymin_input, all_hydro_sites_historical_filtered$q5, 
                                    all_hydro_sites_1yr_filtered$Value, multiplier = 0.96, direction = "min")
    
    
    # get plot breaks sequence
    break_seq <- get_custom_plot_seq(custom_ymax, custom_ymin)
    
    
    # code to check that historical df isn't empty
    if (nrow(all_hydro_sites_historical_filtered) > 0 && !all(is.na(all_hydro_sites_historical_filtered$arbitrary_date))) {
      
      # Fill in missing dates with NA values for historical data
      all_hydro_sites_historical_filtered <- all_hydro_sites_historical_filtered %>%
        tidyr::complete(arbitrary_date = seq.Date(min(arbitrary_date, na.rm = TRUE),
                                                  max(arbitrary_date, na.rm = TRUE), by = "day"))
      
    }
    
    # Code to check YOI df isn't empty
    if (nrow(all_hydro_sites_1yr_filtered) > 0 && !all(is.na(all_hydro_sites_1yr_filtered$Date))) {
      
      # Fill in missing dates with NA values for current year data
      all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr_filtered %>%
        tidyr::complete(arbitrary_date = seq.Date(min(arbitrary_date, na.rm = TRUE), 
                                                  max(arbitrary_date, na.rm = TRUE), by = "day"))
      
    }
    
    # Checking if all values in 1-year data are NA
    year_has_data <- !(all(is.na(all_hydro_sites_1yr_filtered$Value)))
    
    
    # Checking if all values in the key columns are NA for historical data
    historical_has_data <- !(all(is.na(all_hydro_sites_historical_filtered$q5)) & 
                               all(is.na(all_hydro_sites_historical_filtered$q95)) & 
                               all(is.na(all_hydro_sites_historical_filtered$q25)) & 
                               all(is.na(all_hydro_sites_historical_filtered$q75)) & 
                               all(is.na(all_hydro_sites_historical_filtered$median)) & 
                               all(is.na(all_hydro_sites_historical_filtered$mean)))
    
    
    # starting plotting ----------------------------------------------------------------------------------
    
    plot_caption <- sprintf("Date Range for historical stats: %s", unique(all_hydro_sites_historical_filtered$year_ranges))
    
    
    # Initialize ggplot object
    hydrograph <- ggplot()
    
    # Add historical ribbons and lines if there is data
    if (historical_has_data) {
      hydrograph <- hydrograph +
        geom_ribbon(all_hydro_sites_historical_filtered, 
                    mapping = aes(ymin = q5, ymax = q95, x = arbitrary_date, fill = "q5-q95"), 
                    alpha = 0.75) +
        geom_ribbon(all_hydro_sites_historical_filtered, 
                    mapping = aes(ymin = q25, ymax = q75, x = arbitrary_date, fill = "q25-q75"), 
                    alpha = 0.8) +
        geom_line(all_hydro_sites_historical_filtered, 
                  mapping = aes(x = arbitrary_date, y = median, color = "Median"), linewidth = 0.6) +
        geom_line(all_hydro_sites_historical_filtered, 
                  mapping = aes(x = arbitrary_date, y = mean, color = "Mean"), linewidth = 0.6)
    } else {
      warning("All historical data columns are NA, skipping historical data plotting.")
    }
    
    # Add 1-year line if there is data
    if (year_has_data) {
      hydrograph <- hydrograph +
        geom_line(all_hydro_sites_1yr_filtered, 
                  mapping = aes(x = arbitrary_date, y = Value, color = as.character(year_label)),
                  linewidth = 0.7)
    } else {
      warning("All 1-year data values are NA, skipping 1-year data plotting.")
    }
    
    
    plot_colours <- setNames(c( "black", "beige", "deeppink4" ), 
                             c("Median", "Mean", as.character(year_label)))
    
    # Add common plot elements
    hydrograph <- hydrograph + 
      theme_bw() +
      scale_linetype_manual(values = c(2), name = element_blank()) +
      scale_x_date(date_labels = "%b", 
                   breaks = "1 month", 
                   name = "Date") +
      scale_y_continuous(breaks = break_seq,
                         limits = c(custom_ymin, custom_ymax),
                         name = y_lab) +
      scale_fill_manual(values = c("q25-q75" = "lightblue4", "q5-q95" = "lightblue3"), 
                        name = element_blank()) +
      scale_color_manual(values = plot_colours, 
                         name = "Daily Statistics") +
      theme(panel.grid.minor.x = element_blank(), 
            legend.margin = margin(t = -15, 0, 0, 0), 
            legend.key.height = unit(0.7, "cm")) +
      guides(color = guide_legend(order = 1), 
             fill = guide_legend(order = 2)) +
      labs(caption = plot_caption)
    
    # if figure represents flow add Mean Annual Discharge line
    if (tolower(parameter) == "flow"){
      
      hydrograph <- hydrograph +   
        geom_hline(aes(yintercept = mean(all_hydro_sites_historical_filtered$mean),linetype = "MAD"))
      
    }
    
    # Print the plot if there's valid data, or handle the case where nothing is plotted
    if (historical_has_data | year_has_data) {
      
      # if outputting graph into a .jpeg, add a figure title to easily know which station the figure represents
      if (tolower(output_type) == "jpeg"){
        
        hydrograph <- hydrograph + 
          ggtitle(locations[location_num])
        
        file_path <- sprintf("figures/%s_%s_%s_hydrograph.jpeg", stringr::str_remove(locations[location_num], " "), WY_label, parameter)
        ggsave(file_path, plot = hydrograph, width = 9, height = 5.5, create.dir = T)
        simpleMessage(sprintf("Figure saved to %s", file_path))
        
        
      }else if(tolower(output_type) == "print"){
        
        print(hydrograph)
        
      # }else if(tolower(output_type) == "plotly"){
      #   
      #   hydrograph <- plotly::ggplotly(hydrograph)
      #   
      #   print(hydrograph)
      #   
      # }else if(tolower(output_type) == "dygraph"){
      #   
      #   # Prepare the data for dygraphs
      #   if (historical_has_data || year_has_data) {
      #     
      #     # Combine the data into a single data frame
      #     combined_data <- data.frame(
      #       Date = all_hydro_sites_historical_filtered$arbitrary_date,
      #       Q5 = all_hydro_sites_historical_filtered$q5,
      #       Q95 = all_hydro_sites_historical_filtered$q95,
      #       Q25 = all_hydro_sites_historical_filtered$q25,
      #       Q75 = all_hydro_sites_historical_filtered$q75,
      #       Median = all_hydro_sites_historical_filtered$median,
      #       Mean = all_hydro_sites_historical_filtered$mean,
      #       CurrentYear = if (year_has_data) all_hydro_sites_1yr_filtered$Value else NA
      #     )
      #     
      #     # Convert to xts object for dygraphs
      #     combined_xts <- xts::xts(combined_data[, -1], order.by = combined_data$Date)
      #     
      #     # Create dygraph
      #     interactive_hydrograph <- dygraphs::dygraph(combined_xts, main = "Interactive Hydrograph") %>%
      #       dygraphs::dySeries(c("Q5", "Q95"), label = "q5-q95 Range", fillGraph = TRUE, color = "lightblue3") %>%
      #       dygraphs::dySeries(c("Q25", "Q75"), label = "q25-q75 Range", fillGraph = TRUE, color = "lightblue4") %>%
      #       dygraphs::dySeries("Median", label = "Median", color = "black") %>%
      #       dygraphs::dySeries("Mean", label = "Mean", color = "beige") %>%
      #       dygraphs::dySeries("CurrentYear", label = as.character(year_label), color = "deeppink4") %>%
      #       dygraphs::dyAxis("x", label = "Date") %>%
      #       dygraphs::dyAxis("y", label = y_lab) %>%
      #       dygraphs::dyLegend(show = "always") %>%
      #       dygraphs::dyRangeSelector() # Adds range selector for zooming
        # } else {
        #   base::warning("No valid data available for plotting.")
        #   interactive_hydrograph <- NULL
        # }
        # 
        # # Print the interactive graph
        # print(interactive_hydrographload)
        
      }else{
        stop("Incorrect input to output_type. Please enter either print or jpeg and try again")
        
      }
      
    } else {
      warning("No valid data available to plot.")
    }
    
    
  }
  
}  



# ----------------------------------------------------------------------------------


#' @export
#' @rdname create_hydrograph_separate
create_hydrograph_faceted <- function(all_hydro_sites_historical, all_hydro_sites_1yr, 
                                      parameter = "flow", 
                                      WY = FALSE, 
                                      output_type = "print", 
                                      fixed_y_scales = "fixed", 
                                      custom_ymax_input = NA,
                                      custom_ymin_input = NA, 
                                      jpeg_width = 6, 
                                      jpeg_height = 8){
  
  # filter df's for parameter wanted
  param_info <- select_hydro_parameter(all_hydro_sites_historical, all_hydro_sites_1yr, parameter)
  
  #pull output values from function  
  all_hydro_sites_historical <- param_info[[1]]
  all_hydro_sites_1yr <- param_info[[2]]
  y_lab <- param_info[[3]]
  
  
  # set up object for a fixed or free y scale
  if(fixed_y_scales == "fixed"){
    scale_type <- "fixed"
  }else if(fixed_y_scales == "free"){
    scale_type <- "free_y"
  }else{
    stop("Please enter either 'fixed' or 'free' for fixed_y_scales.")
  }
  
  
  locations <- unique(all_hydro_sites_historical$STATION_NUMBER)
  
  plot_list <- list()
  
  for (plot_num in length(unique(all_hydro_sites_historical$STATION_NUMBER)):1){
    
    if(is.logical(WY)){
      if (WY){
        
        # dplyr::rename arbitrary_date so it's plotted properly
        all_hydro_sites_historical_filtered <- all_hydro_sites_historical %>%
          dplyr::select(-arbitrary_date) %>%
          dplyr::rename(arbitrary_date = arbitrary_date_WY)
        
        all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr %>% 
          dplyr::select(-arbitrary_date) %>%
          dplyr::rename(arbitrary_date = arbitrary_date_WY)
        
        current_year <- lubridate::year(max(all_hydro_sites_1yr_filtered$Date))
        
        years_in_dataset <- unique(lubridate::year(all_hydro_sites_1yr_filtered$Date))
        
        previous_yr <- current_year -1
        
        if (!is.na(previous_yr) &&  !(previous_yr %in% years_in_dataset)){
          warning(sprintf(
            "The data set for station %s is missing data from October to December for the current WY. 
          This may be due to not plotting the data in water years (WY = 'no') in the create_hydro_stats_singleYr function.
          If you intended to display in WY, please re-run the create_hydro_stats_singleYr function with WY set to 'yes'.
          Otherwise, it may just be that your dataset is missing data for this period. ",
            locations[plot_num]
          ))
        }
        
        year_label <- max(unique(all_hydro_sites_1yr$WY_col))
        
      }else{
        
        all_hydro_sites_historical_filtered <- all_hydro_sites_historical 
        
        all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr 
        
        year_label <- max(unique(all_hydro_sites_1yr$year_col))
        
      }
      
    }else{
      stop("Please enter a valid logical value for WY")
    }
    

    
    

    

    # calculate limits of x/y axis
    custom_ymax <- set_custom_limit(custom_ymax_input, all_hydro_sites_historical_filtered$q95, 
                                    all_hydro_sites_1yr_filtered$Value, multiplier = 1.04, direction = "max")
    
    custom_ymin <- set_custom_limit(custom_ymin_input, all_hydro_sites_historical_filtered$q5, 
                                    all_hydro_sites_1yr_filtered$Value, multiplier = 0.96, direction = "min")
    
    break_seq <- get_custom_plot_seq(custom_ymax, custom_ymin)
    
    
    all_hydro_sites_historical_filtered_mean <- all_hydro_sites_historical_filtered %>%
      dplyr::group_by(STATION_NUMBER) %>%
      dplyr::summarise(location_mean = mean(mean))
    
    
    
    plot_colours <- setNames(c("deeppink4", "black", "beige" ), 
                        c(year_label, "Median", "Mean"))
    
    
    hydrograph <-
      ggplot() + 
      #geom_ribbon(all_hydro_sites_historical, mapping = aes(ymin = min, ymax = max, x = arbitrary_date, fill = "Minimum-Maximum")) +
      geom_ribbon(all_hydro_sites_historical_filtered, mapping = aes(ymin = q5, ymax = q95, x = arbitrary_date, fill = "q5-95"), 
                  alpha = 0.75) +
      geom_ribbon(all_hydro_sites_historical_filtered, mapping = aes(ymin = q25, ymax = q75, x = arbitrary_date, fill = "q25-75"), 
                  alpha = 0.8) +
      geom_line(all_hydro_sites_historical_filtered, mapping = aes(x = arbitrary_date, y = median, color = "Median"), linewidth = 0.6) +
      geom_line(all_hydro_sites_historical_filtered, mapping = aes(x = arbitrary_date, y = mean, color = "Mean"), linewidth = 0.6) +
      geom_line(all_hydro_sites_1yr_filtered, mapping = aes(x =arbitrary_date, y = Value, color = as.character(year_label)), linewidth = 0.7) + 
      geom_hline(all_hydro_sites_historical_filtered_mean, mapping = aes(yintercept = location_mean, linetype = "MAD"))+
      theme_bw() +
      scale_linetype_manual(values = c(2), name = element_blank()) +
      
      scale_x_date(date_labels = "%b", breaks = "1 month",  name = "Date") +
      scale_y_continuous( name = y_lab) +
      scale_fill_manual(values = c(#"lightblue2", 
        "lightblue4", 
        "lightblue3"), name = element_blank()) +
      scale_color_manual(values = plot_colours, 
                         name = "Daily Statistics") +
      facet_grid(STATION_NUMBER~., scales = scale_type) +
      theme(panel.grid.minor.x =  element_blank(), 
            legend.margin = margin(t = -15, 0, 0, 0), 
            legend.key.height = unit(0.7, "cm")) +
      guides(color = guide_legend(order = 1), 
             fill = guide_legend(order =2))
  }
  
  # if outputting graph into a .jpeg, add a figure title to easily know which station the figure represents
  if (tolower(output_type) == "jpeg"){

    
    file_path <- sprintf("figures/hydrograph_faceted_%s.jpeg",
                         purrr::reduce(lapply(locations, stringr::str_remove, pattern = " "), ~paste(.x, .y, sep = "_"))) # for each STATION_NUMBER in list remove the spaces in the name and then join all strings with an understore between
    
    ggsave(file_path, plot = hydrograph, width = jpeg_width, height = jpeg_height, create.dir = T)
    simpleMessage(sprintf("Figure saved to %s", file_path))
    
    
  }else if(tolower(output_type) == "print"){
    return(hydrograph)
  }else{
    stop("Incorrect input to output_type. Please enter either print or jpeg and try again")
    
  }
  
  
  
}


