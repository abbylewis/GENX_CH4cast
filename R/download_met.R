#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
library(tidyverse)

#Identify all files
files <- drop_dir(path = "GCREW_LOGGERNET_DATA")
relevant_files <- files %>%
  filter(grepl("GCREW_MET_GCREW_MET", name))
current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
  filter(grepl("GCREW_MET_GCREW_MET", name),
         !grepl("backup", name))

#Remove files that are already loaded
already_loaded <- list.files("./Raw_data/dropbox_met")
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
    httr::write_disk(paste0("./Raw_data/dropbox_met/", name), overwrite = T)
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

data <- list.files("./Raw_data/dropbox_met", full.names = T) %>%
  map(read.csv, skip = 1) %>%
  bind_rows() %>%
  filter(!TIMESTAMP == "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  filter(!is.na(TIMESTAMP)) %>%
  distinct()

met_output <- data %>%
  mutate(TIMESTAMP = with_tz(TIMESTAMP, tzone = "America/New_York"),
         Date = as.Date(TIMESTAMP)) %>%
  group_by(Date) %>%
  select(Date, PAR_Den_C_Avg, Rain_cm_Tot, Barometric_Pressure_PB110B, RH, WS_ms_RM_Avg, SlrW_Avg) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(Rain_cm_Tot = sum(Rain_cm_Tot, na.rm = TRUE)) %>% #Daily sums for rain
  summarize(across(is.numeric, mean, na.rm = TRUE))

data %>%
  ggplot(aes(x = par_den_c_avg, y = par_tot_c_tot)) +
  geom_point()

data %>%
  ggplot(aes(x = ws_ms_rm_avg, y = ws_ms_ws_avg)) +
  geom_point()

data %>%
  ggplot(aes(x = slrw_avg, y = par_den_c_avg, color = time2)) +
  geom_point()

write.csv(met_output %>% rename(PAR = par_den_c_avg) %>% select(Date, PAR), 
          "../processed_data/PAR_output.csv", row.names = FALSE)

write.csv(met_output,
          "../processed_data/met_output.csv", row.names = FALSE)