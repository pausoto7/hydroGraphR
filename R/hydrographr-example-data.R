#' Access Example Data Files in hydroGraphR
#'
#' This utility function provides easy access to example data files included with 
#' the `hydroGraphR` package, located in its `inst/extdata` directory.
#' 
#' Adapted from https://readxl.tidyverse.org/reference/readxl_example.html
#'
#' @param path Character string specifying the name of the example file. If `NULL`, 
#' all example files will be listed. Default is `NULL`.
#' 
#' @return A character vector containing either the full path to the specified file 
#' or a list of all available example files if `path` is `NULL`.
#' @examples
#' get_example_file()
#' get_example_file("kamloops_aut_1163842_1.csv")
#' 
#' files <- get_example_file()
#' get_example_file(grep("merritt_", files, value = TRUE))
#' 
#' @export
get_example_file <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "hydroGraphR"))
  } else {
    system.file("extdata", path, package = "hydroGraphR", mustWork = TRUE)
  }
}