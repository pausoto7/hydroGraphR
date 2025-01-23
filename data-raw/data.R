all_files <- lapply(list.files("data-raw", pattern = "merritt_", full.names = TRUE), read.csv)
climate_raw <- do.call(rbind, all_files)
usethis::use_data(climate_raw, overwrite = TRUE)
