#' Create Hydrograph Plots
#'
#' @description
#' - **`create_hydrograph_separate`**: Generates separate hydrograph plots for specified locations.
#' - **`create_hydrograph_faceted`**: Creates a joint hydrograph plot for specified locations using `facet_grid`.
#'
#' @import ggplot2
#' @param all_hydro_sites_historical A data frame containing historical hydrology data.
#' @param all_hydro_sites_1yr A data frame containing one-year hydrology data. Can be NULL/NA to plot historical-only.
#' @param parameter A character string specifying the parameter to plot. Options are `"flow"` or `"level"`.
#' @param output_type A character string indicating the output format. Options are `"JPEG"` or `"print"`.
#' @param WY Logical value indicating whether to present hydrograph by water year (`TRUE`) or calendar year (`FALSE`).
#' @param fixed_y_scales `"fixed"` or `"free"` across facets.
#' @param custom_ymax_input Custom ymax (NA for auto).
#' @param custom_ymin_input Custom ymin (NA for auto).
#' @param jpeg_width Width (inches) for JPEG output.
#' @param jpeg_height Height (inches) for JPEG output.
#'
#' @return Hydrograph plots saved to `"figures/"` folder or returned/printed.
#'
#' @export
create_hydrograph_separate <- function(all_hydro_sites_historical,
                                       all_hydro_sites_1yr = NULL,
                                       parameter,
                                       output_type = "print",
                                       WY = FALSE,
                                       custom_ymax_input = NA,
                                       custom_ymin_input = NA) {
  
  # ---- allow NULL/NA for 1yr (historical-only mode) ----
  one_year_missing <- is.null(all_hydro_sites_1yr) ||
    (length(all_hydro_sites_1yr) == 1 && is.na(all_hydro_sites_1yr))
  
  # keep columns stable for downstream helpers
  if (one_year_missing) {
    all_hydro_sites_1yr <- all_hydro_sites_historical[0, , drop = FALSE]
  }
  
  if (!is.logical(WY)) stop("Please enter a valid logical value for WY")
  WY_label <- if (WY) "WY" else "calendar"
  
  locations <- unique(all_hydro_sites_historical$STATION_NUMBER)
  
  # filter df's for parameter wanted
  param_info <- select_hydro_parameter(all_hydro_sites_historical, all_hydro_sites_1yr, parameter)
  
  all_hydro_sites_historical <- param_info[[1]]
  all_hydro_sites_1yr <- param_info[[2]]
  y_lab <- param_info[[3]]
  
  for (location_num in seq_along(locations)) {
    message("Processing station: ", locations[location_num], "\n")
    
    # ---- choose timeline / filter per station ----
    if (one_year_missing || nrow(all_hydro_sites_1yr) == 0) {
      
      # historical-only: filter to station + rename arbitrary_date if WY
      all_hydro_sites_historical_filtered <- all_hydro_sites_historical[
        all_hydro_sites_historical$STATION_NUMBER == locations[location_num],
        ,
        drop = FALSE
      ]
      
      if (WY && "arbitrary_date_WY" %in% names(all_hydro_sites_historical_filtered)) {
        all_hydro_sites_historical_filtered <- all_hydro_sites_historical_filtered %>%
          dplyr::select(-arbitrary_date) %>%
          dplyr::rename(arbitrary_date = arbitrary_date_WY)
      }
      
      all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr[0, , drop = FALSE]
      year_label <- NA
      
    } else {
      
      # historical + 1yr path (original behaviour)
      timeline_metadata <- withCallingHandlers(
        choose_hydro_timeline(all_hydro_sites_historical, all_hydro_sites_1yr, WY, locations, location_num),
        warning = function(w) {
          if (grepl("all_hydro_sites_1yr is empty. Historical values cannot be compared with this year", w$message)) {
            invokeRestart("muffleWarning")
          }
        }
      )
      
      all_hydro_sites_historical_filtered <- timeline_metadata[[1]]
      all_hydro_sites_1yr_filtered <- timeline_metadata[[2]]
      year_label <- timeline_metadata[[3]]
    }
    
    # ---- axis limits (safe when 1yr empty) ----
    yoi_vals <- if (nrow(all_hydro_sites_1yr_filtered) > 0) all_hydro_sites_1yr_filtered$Value else numeric(0)
    
    custom_ymax <- set_custom_limit(
      custom_ymax_input,
      all_hydro_sites_historical_filtered$q95,
      yoi_vals,
      multiplier = 1.04,
      direction = "max"
    )
    
    custom_ymin <- set_custom_limit(
      custom_ymin_input,
      all_hydro_sites_historical_filtered$q5,
      yoi_vals,
      multiplier = 0.96,
      direction = "min"
    )
    
    break_seq <- get_custom_plot_seq(custom_ymax, custom_ymin)
    
    # ---- fill missing dates (historical) ----
    if (nrow(all_hydro_sites_historical_filtered) > 0 &&
        "arbitrary_date" %in% names(all_hydro_sites_historical_filtered) &&
        !all(is.na(all_hydro_sites_historical_filtered$arbitrary_date))) {
      
      all_hydro_sites_historical_filtered <- all_hydro_sites_historical_filtered %>%
        tidyr::complete(
          arbitrary_date = seq.Date(
            min(arbitrary_date, na.rm = TRUE),
            max(arbitrary_date, na.rm = TRUE),
            by = "day"
          )
        )
    }
    
    # ---- fill missing dates (1yr) ----
    if (nrow(all_hydro_sites_1yr_filtered) > 0 &&
        "arbitrary_date" %in% names(all_hydro_sites_1yr_filtered) &&
        !all(is.na(all_hydro_sites_1yr_filtered$arbitrary_date))) {
      
      all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr_filtered %>%
        tidyr::complete(
          arbitrary_date = seq.Date(
            min(arbitrary_date, na.rm = TRUE),
            max(arbitrary_date, na.rm = TRUE),
            by = "day"
          )
        )
    }
    
    # ---- data presence checks ----
    year_has_data <- nrow(all_hydro_sites_1yr_filtered) > 0 &&
      "Value" %in% names(all_hydro_sites_1yr_filtered) &&
      !all(is.na(all_hydro_sites_1yr_filtered$Value))
    
    historical_has_data <- nrow(all_hydro_sites_historical_filtered) > 0 &&
      !(all(is.na(all_hydro_sites_historical_filtered$q5)) &
          all(is.na(all_hydro_sites_historical_filtered$q95)) &
          all(is.na(all_hydro_sites_historical_filtered$q25)) &
          all(is.na(all_hydro_sites_historical_filtered$q75)) &
          all(is.na(all_hydro_sites_historical_filtered$median)) &
          all(is.na(all_hydro_sites_historical_filtered$mean)))
    
    # ---- caption (same as before) ----
    plot_caption <- sprintf(
      "Date Range for historical stats: %s",
      unique(all_hydro_sites_historical_filtered$year_ranges)
    )
    
    # ---- build plot ----
    hydrograph <- ggplot()
    
    if (historical_has_data) {
      hydrograph <- hydrograph +
        geom_ribbon(
          all_hydro_sites_historical_filtered,
          mapping = aes(ymin = q5, ymax = q95, x = arbitrary_date, fill = "q5-q95"),
          alpha = 0.75
        ) +
        geom_ribbon(
          all_hydro_sites_historical_filtered,
          mapping = aes(ymin = q25, ymax = q75, x = arbitrary_date, fill = "q25-q75"),
          alpha = 0.8
        ) +
        geom_line(
          all_hydro_sites_historical_filtered,
          mapping = aes(x = arbitrary_date, y = median, color = "Median"),
          linewidth = 0.6
        ) +
        geom_line(
          all_hydro_sites_historical_filtered,
          mapping = aes(x = arbitrary_date, y = mean, color = "Mean"),
          linewidth = 0.6
        )
    } else {
      warning(sprintf(
        "All historical data columns are NA for station %s, skipping historical data plotting.",
        locations[location_num]
      ))
    }
    
    if (year_has_data) {
      hydrograph <- hydrograph +
        geom_line(
          all_hydro_sites_1yr_filtered,
          mapping = aes(x = arbitrary_date, y = Value, color = as.character(year_label)),
          linewidth = 0.7
        )
    }
    
    # ---- KEEP AESTHETICS STABLE: colours + legend order ----
    year_label_chr <- NULL
    if (year_has_data && !is.na(year_label)) year_label_chr <- as.character(year_label)
    
    plot_colours <- c("Median" = "black", "Mean" = "beige")
    if (!is.null(year_label_chr)) {
      plot_colours <- c(plot_colours, setNames("deeppink4", year_label_chr))
    }
    
    colour_breaks <- c("Median", "Mean")
    if (!is.null(year_label_chr)) colour_breaks <- c(colour_breaks, year_label_chr)
    
    hydrograph <- hydrograph +
      theme_bw() +
      scale_linetype_manual(values = c(2), name = element_blank()) +
      scale_x_date(date_labels = "%b", breaks = "1 month", name = "Date") +
      scale_y_continuous(
        breaks = break_seq,
        limits = c(custom_ymin, custom_ymax),
        name = y_lab
      ) +
      scale_fill_manual(
        values = c("q25-q75" = "lightblue4", "q5-q95" = "lightblue3"),
        name = element_blank()
      ) +
      scale_color_manual(
        values = plot_colours,
        breaks = colour_breaks,
        name = "Daily Statistics"
      ) +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.margin = margin(t = -15, 0, 0, 0),
        legend.key.height = unit(0.7, "cm")
      ) +
      guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
      labs(caption = plot_caption)
    
    # if figure represents flow add Mean Annual Discharge line (unchanged)
    if (tolower(parameter) == "flow" && historical_has_data) {
      hydrograph <- hydrograph +
        geom_hline(aes(yintercept = mean(all_hydro_sites_historical_filtered$mean), linetype = "MAD"))
    }
    
    # output (unchanged)
    if (historical_has_data || year_has_data) {
      
      if (tolower(output_type) == "jpeg") {
        
        hydrograph <- hydrograph + ggtitle(locations[location_num])
        
        file_path <- sprintf(
          "figures/%s_%s_%s_hydrograph.jpeg",
          stringr::str_remove(locations[location_num], " "),
          WY_label,
          parameter
        )
        
        ggsave(file_path, plot = hydrograph, width = 9, height = 5.5, create.dir = TRUE)
        simpleMessage(sprintf("Figure saved to %s", file_path))
        
      } else if (tolower(output_type) == "print") {
        
        print(hydrograph)
        
      } else {
        stop("Incorrect input to output_type. Please enter either print or jpeg and try again")
      }
    }
  }
}



# ----------------------------------------------------------------------------------

#' @export
#' @rdname create_hydrograph_separate
create_hydrograph_faceted <- function(all_hydro_sites_historical,
                                      all_hydro_sites_1yr = NULL,
                                      parameter = "flow",
                                      WY = FALSE,
                                      output_type = "print",
                                      fixed_y_scales = "fixed",
                                      custom_ymax_input = NA,
                                      custom_ymin_input = NA,
                                      jpeg_width = 6,
                                      jpeg_height = 8) {
  
  one_year_missing <- is.null(all_hydro_sites_1yr) ||
    (length(all_hydro_sites_1yr) == 1 && is.na(all_hydro_sites_1yr))
  
  if (one_year_missing) {
    all_hydro_sites_1yr <- all_hydro_sites_historical[0, , drop = FALSE]
  }
  
  all_hydro_sites_historical_initial <- all_hydro_sites_historical
  all_hydro_sites_1yr_initial <- all_hydro_sites_1yr
  
  # filter df's for parameter wanted
  param_info <- select_hydro_parameter(all_hydro_sites_historical, all_hydro_sites_1yr, parameter)
  
  all_hydro_sites_historical <- param_info[[1]]
  all_hydro_sites_1yr <- param_info[[2]]
  y_lab <- param_info[[3]]
  
  # fixed vs free scales (unchanged)
  if (fixed_y_scales == "fixed") {
    scale_type <- "fixed"
  } else if (fixed_y_scales == "free") {
    scale_type <- "free_y"
  } else {
    stop("Please enter either 'fixed' or 'free' for fixed_y_scales.")
  }
  
  if (!is.logical(WY)) stop("Please enter a valid logical value for WY")
  
  locations <- unique(all_hydro_sites_historical$STATION_NUMBER)
  
  # ---- WY handling (guard 1yr transforms) ----
  if (WY) {
    all_hydro_sites_historical_filtered <- all_hydro_sites_historical %>%
      dplyr::select(-arbitrary_date) %>%
      dplyr::rename(arbitrary_date = arbitrary_date_WY)
    
    if (!one_year_missing && nrow(all_hydro_sites_1yr) > 0) {
      all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr %>%
        dplyr::select(-arbitrary_date) %>%
        dplyr::rename(arbitrary_date = arbitrary_date_WY)
      
      # original warning logic (guarded)
      current_year <- lubridate::year(max(all_hydro_sites_1yr_filtered$Date))
      years_in_dataset <- unique(lubridate::year(all_hydro_sites_1yr_filtered$Date))
      previous_yr <- current_year - 1
      
      if (!is.na(previous_yr) && !(previous_yr %in% years_in_dataset)) {
        warning(sprintf(
          "The data set for station %s is missing data from October to December for the current WY.
This may be due to not plotting the data in water years (WY = 'no') in the create_hydro_stats_singleYr function.
If you intended to display in WY, please re-run the create_hydro_stats_singleYr function with WY set to 'yes'.
Otherwise, it may just be that your dataset is missing data for this period.",
          locations[1]
        ))
      }
      
    } else {
      all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr[0, , drop = FALSE]
    }
    
  } else {
    all_hydro_sites_historical_filtered <- all_hydro_sites_historical
    all_hydro_sites_1yr_filtered <- all_hydro_sites_1yr
  }
  
  # ---- year label (safe) ----
  year_label <- NA
  if (!one_year_missing &&
      nrow(all_hydro_sites_1yr_filtered) > 0 &&
      "year_col" %in% names(all_hydro_sites_1yr_filtered) &&
      !all(is.na(all_hydro_sites_1yr_filtered$year_col))) {
    year_label <- max(unique(all_hydro_sites_1yr_filtered$year_col), na.rm = TRUE)
  }
  
  year_has_data <- (!one_year_missing &&
                      nrow(all_hydro_sites_1yr_filtered) > 0 &&
                      "Value" %in% names(all_hydro_sites_1yr_filtered) &&
                      !all(is.na(all_hydro_sites_1yr_filtered$Value)))
  
  # ---- axis limits (safe when 1yr empty) ----
  yoi_vals <- if (year_has_data) all_hydro_sites_1yr_filtered$Value else numeric(0)
  
  custom_ymax <- set_custom_limit(
    custom_ymax_input,
    all_hydro_sites_historical_filtered$q95,
    yoi_vals,
    multiplier = 1.04,
    direction = "max"
  )
  
  custom_ymin <- set_custom_limit(
    custom_ymin_input,
    all_hydro_sites_historical_filtered$q5,
    yoi_vals,
    multiplier = 0.96,
    direction = "min"
  )
  
  break_seq <- get_custom_plot_seq(custom_ymax, custom_ymin)
  
  all_hydro_sites_historical_filtered_mean <- all_hydro_sites_historical_filtered %>%
    dplyr::group_by(STATION_NUMBER) %>%
    dplyr::summarise(location_mean = mean(mean), .groups = "drop")
  
  # ---- KEEP AESTHETICS STABLE: colours + legend order ----
  year_label_chr <- NULL
  if (year_has_data && !is.na(year_label)) year_label_chr <- as.character(year_label)
  
  plot_colours <- c("Median" = "black", "Mean" = "beige")
  if (!is.null(year_label_chr)) {
    plot_colours <- c(setNames("deeppink4", year_label_chr), plot_colours)
  }
  
  # match your original intent (year, then median, then mean) while keeping stable
  colour_breaks <- c()
  if (!is.null(year_label_chr)) colour_breaks <- c(colour_breaks, year_label_chr)
  colour_breaks <- c(colour_breaks, "Median", "Mean")
  
  # ---- base plot (historical aesthetics unchanged) ----
  hydrograph <-
    ggplot() +
    geom_ribbon(
      all_hydro_sites_historical_filtered,
      mapping = aes(ymin = q5, ymax = q95, x = arbitrary_date, fill = "q5-95"),
      alpha = 0.75
    ) +
    geom_ribbon(
      all_hydro_sites_historical_filtered,
      mapping = aes(ymin = q25, ymax = q75, x = arbitrary_date, fill = "q25-75"),
      alpha = 0.8
    ) +
    geom_line(
      all_hydro_sites_historical_filtered,
      mapping = aes(x = arbitrary_date, y = median, color = "Median"),
      linewidth = 0.6
    ) +
    geom_line(
      all_hydro_sites_historical_filtered,
      mapping = aes(x = arbitrary_date, y = mean, color = "Mean"),
      linewidth = 0.6
    ) +
    geom_hline(
      all_hydro_sites_historical_filtered_mean,
      mapping = aes(yintercept = location_mean, linetype = "MAD")
    ) +
    theme_bw() +
    scale_linetype_manual(values = c(2), name = element_blank()) +
    scale_x_date(date_labels = "%b", breaks = "1 month", name = "Date") +
    scale_y_continuous(breaks = break_seq, name = y_lab) +
    scale_fill_manual(values = c("lightblue4", "lightblue3"), name = element_blank()) +
    scale_color_manual(values = plot_colours, breaks = colour_breaks, name = "Daily Statistics") +
    facet_grid(STATION_NUMBER ~ ., scales = scale_type) +
    theme(
      panel.grid.minor.x = element_blank(),
      legend.margin = margin(t = -15, 0, 0, 0),
      legend.key.height = unit(0.7, "cm")
    ) +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))
  
  # ---- add 1yr line ONLY if present (keeps everything else identical) ----
  if (year_has_data && !is.null(year_label_chr)) {
    hydrograph <- hydrograph +
      geom_line(
        all_hydro_sites_1yr_filtered,
        mapping = aes(x = arbitrary_date, y = Value, color = year_label_chr),
        linewidth = 0.7
      )
  }
  
  # ---- missing station warnings (skip 1yr warning when historical-only) ----
  locations_hist <- na.omit(unique(all_hydro_sites_historical_filtered$STATION_NUMBER))
  
  missing_stations_historical <- unique(all_hydro_sites_historical_initial$STATION_NUMBER[
    !all_hydro_sites_historical_initial$STATION_NUMBER %in% locations_hist
  ])
  if (length(missing_stations_historical) > 0) {
    warning("No historical data found for the following locations: ",
            paste(missing_stations_historical, collapse = ", "))
  }
  
  if (!one_year_missing && nrow(all_hydro_sites_1yr_initial) > 0) {
    locations_1yr <- na.omit(unique(all_hydro_sites_1yr_filtered$STATION_NUMBER))
    missing_stations_1yr <- unique(all_hydro_sites_1yr_initial$STATION_NUMBER[
      !all_hydro_sites_1yr_initial$STATION_NUMBER %in% locations_1yr
    ])
    if (length(missing_stations_1yr) > 0) {
      warning("No YOI data found for the following locations: ",
              paste(missing_stations_1yr, collapse = ", "))
    }
  }
  
  # output (unchanged)
  if (tolower(output_type) == "jpeg") {
    
    file_path <- sprintf(
      "figures/hydrograph_faceted_%s.jpeg",
      purrr::reduce(
        lapply(locations, stringr::str_remove, pattern = " "),
        ~ paste(.x, .y, sep = "_")
      )
    )
    
    ggsave(file_path, plot = hydrograph, width = jpeg_width, height = jpeg_height, create.dir = TRUE)
    simpleMessage(sprintf("Figure saved to %s", file_path))
    
  } else if (tolower(output_type) == "print") {
    
    return(hydrograph)
    
  } else {
    stop("Incorrect input to output_type. Please enter either print or jpeg and try again")
  }
}
