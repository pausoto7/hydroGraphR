#' Raw daily climate data for station MERRITT STP
#'
#' Example *daily* climate data acquired from the Canadian climate extraction tool 
#' (https://climate-change.canada.ca/climate-data/#/hourly-climate-data)
#' 
#' This data frame can be used in [clean_climate()].
#'
#' @format ## `who`
#' A data frame with 18,945 rows and 36 columns:
#' \describe{
#'   \item{x, y}{latitude and longitude in decimal degrees}
#'   \item{SPEED_MAX_GUST_FLAG}{etc etc}
#'   \item{MIN_TEMPERATURE}{etc etc}
#'   ...
#' }
#' @source <https://climate-change.canada.ca/climate-data/#/hourly-climate-data>
"climate_raw"