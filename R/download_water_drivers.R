#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
library(tidyverse)

#Identify all files
files <- drop_dir(path = "GCREW_LOGGERNET_DATA")
relevant_files <- files %>%
  filter(grepl("GENX_WaterLevel", name))
current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
  filter(grepl("GENX_WaterLevel", name),
         !grepl("backup", name))

#Remove files that are already loaded
already_loaded <- list.files("./Raw_data/dropbox_water_level")
relevant_files <- relevant_files %>%
  filter(!name %in% already_loaded)

#Load files
load_file <- function(path_display){
  url <- "https://content.dropboxapi.com/2/files/download"
  name <- sub("/GCREW_LOGGERNET_DATA/", "", path_display)
  if(grepl("current", name)) name <- "current.dat"
  
  httr::POST(
    url = url,
    httr::config(token = get_dropbox_token()),
    httr::add_headers("Dropbox-API-Arg" = jsonlite::toJSON(
      list(
        path = path_display
      ),
      auto_unbox = TRUE
    )),
    httr::write_disk(paste0("./Raw_data/dropbox_water_level/", name), overwrite = T)
  )
}

#Load current data
new <- current$path_display %>%
  map(load_file)

if(nrow(relevant_files) == 0){
  message("No new files to download")
} else {
  message("Downloading ", nrow(relevant_files), " files")
  all_data <- relevant_files$path_display %>%
    map(load_file)
}

data <- list.files("./Raw_data/dropbox_water_level", full.names = T) %>%
  map(read.csv, skip = 1) %>%
  bind_rows() %>%
  filter(!TIMESTAMP == "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  filter(!is.na(TIMESTAMP)) %>%
  distinct()

wl_output <- data %>%
  mutate(TIMESTAMP = with_tz(TIMESTAMP, tzone = "America/New_York"),
         Date = as.Date(TIMESTAMP)) %>%
  group_by(Date) %>%
  select(any_of(c("Date", "Depth", "Temperature", "Actual_Conductivity", 
                  "Specific_Conductivity", "Salinity", "TDS", "Water_Density",
                  "Pressure", "Resistivity"))) %>%
  mutate(across(everything(), as.numeric)) %>%
  summarize(across(is.numeric, mean, na.rm = TRUE)) %>%
  filter(Date >= "2000-01-01") %>% #One date was recorded incorrectly
  mutate(corrected_depth = Depth - 76)

wl_output_15min <- data %>%
  mutate(time2 = with_tz(TIMESTAMP, tzone = "America/New_York")) %>%
  select(any_of(c("time2", "Depth", "Temperature", "Actual_Conductivity", 
                  "Specific_Conductivity", "Salinity", "TDS", "Water_Density",
                  "Pressure", "Resistivity"))) %>%
  mutate(across(-time2, as.numeric)) %>%
  filter(time2 >= "2000-01-01") %>% #One date was recorded incorrectly
  mutate(corrected_depth = Depth - 76)

write_csv(wl_output, "./processed_data/WaterLevel_output.csv")

write_csv(wl_output_15min, "./processed_data/WaterLevel_output_15min.csv")
