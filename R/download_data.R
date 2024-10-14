#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
library(tidyverse)

#Identify all files
files <- drop_dir(path = "GCREW_LOGGERNET_DATA")
relevant_files <- files %>%
  filter(grepl("GENX_INSTRUMENT_FLUX_COMB", name) |
           grepl("GENX_FLUX_", name) |
           grepl("GENX_LGR_FLUX_", name))
current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
  filter(grepl("GENX_INSTRUMENT_FLUX_COMB", name),
         !grepl("backup", name))

#Remove files that are already loaded
already_loaded <- list.files("./Raw_data/dropbox_downloads")
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
    httr::write_disk(paste0("./Raw_data/dropbox_downloads/", name), overwrite = T)
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

data_raw <- list.files("./Raw_data/dropbox_downloads", full.names = T) %>%
  map(read_csv, col_types = cols(.default = "c"), skip = 1)  %>%
  bind_rows() %>%
  filter(!TIMESTAMP == "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  filter(!is.na(TIMESTAMP)) %>%
  distinct() 
data <- data_raw %>%
  filter(is.na(LGR_Time) |
           !duplicated(LGR_Time) | 
           !duplicated(CH4_ppm)) #I've spent some time looking into this and there are some duplicated LGR rows
data_small <- data %>%
  select(TIMESTAMP, CH4_ppm, CO2_ppm, MIU_VALVE, GasT_C)
rm(data)

#Create function to assign groups for separate readings
group_fun <- function(MIU_VALVE) {
  group <- rep(1, length(MIU_VALVE))
  for (i in 2:length(MIU_VALVE)) {
    if(MIU_VALVE[i] == MIU_VALVE[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}

### Calculate slopes ###
#Prep data
index_cutoff <- 30 #Higher would be better, but in the summer we wouldn't have enough data to calculate fluxes
filtered_data <- data_small %>%
  mutate(CH4_ppm = as.numeric(CH4_ppm),
         CO2_ppm = as.numeric(CO2_ppm),
         GasT_C = as.numeric(GasT_C),
         MIU_VALVE = as.numeric(MIU_VALVE)) %>%
  filter(!MIU_VALVE == 16,
         !is.na(MIU_VALVE)) %>%
  arrange(TIMESTAMP) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE) %>%
  mutate(start = min(TIMESTAMP),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         index = row_number(),
         n = sum(index > index_cutoff & !is.na(CH4_ppm))) %>%
  filter(index > index_cutoff,
         n >= 5, #need at least 5 data points to calculate slope
         n < 75 #probably some issue if this many measurements are taken
         ) %>%
  #Issues during this window (concs > 100000 ppm)
  filter(TIMESTAMP > "2024-07-09 15:00:00" | TIMESTAMP < "2024-07-09 6:00:00")

#Look at data to confirm index is okay
filtered_data %>%
  ggplot(aes(x = index, y = CH4_ppm, group = start)) +
  geom_line(alpha = 0.2)+
  facet_wrap(~MIU_VALVE)+
  theme(legend.position = "none")

#Calculate slopes
slopes <- filtered_data %>%
  summarize(CH4_slope_ppm_per_day = lm(CH4_ppm ~ change)$coefficients[[2]],
            air_temp = mean(GasT_C, na.rm = T),
            CH4_slope_umol_per_day = CH4_slope_ppm_per_day * 265.8 / (0.08206*(air_temp + 273)),
            CH4_R2 = summary(lm(CH4_ppm ~ change))$r.squared,
            CH4_p = summary(lm(CH4_ppm ~ change))$coefficients[,4][2],
            CH4_rmse = sqrt(mean(lm(CH4_ppm ~ change)$residuals^2)/n()),
            #CO2_slope = lm(CO2_ppm ~ change)$coefficients[[2]],
            #CO2_R2 = summary(lm(CO2_ppm ~ change))$r.squared,
            CH4_init = first(CH4_ppm),
            TIMESTAMP = unique(start),
            n = unique(n))

write.csv(slopes, "L0.csv", row.names = FALSE)

### QAQC ###

#Remove the worst 1% of fits
fit_cutoff <- quantile(slopes$CH4_rmse, 0.99)
#These poor fits have extreme values
slopes %>%
  ggplot(aes(x = CH4_init, y = CH4_slope_umol_per_day, color = CH4_rmse > fit_cutoff)) +
  geom_point() +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5)

slopes_clean <- slopes %>%
  filter(CH4_rmse < fit_cutoff,
         CH4_init > 1.5,
         CH4_init < 2.5)

#R2 is not a good way to filter because this preferentially removes values near 0
slopes_clean %>%
  ggplot(aes(x = CH4_init, y = CH4_slope_umol_per_day, color = CH4_R2)) +
  geom_point()

#High p-values are all close to 0 (good)
slopes_clean %>%
  ggplot(aes(x = CH4_init, y = CH4_slope_umol_per_day, color = CH4_p)) +
  geom_point() 

metadata <- read_csv("Raw_data/chamber_metadata.csv", show_col_types = F)
slopes_metadata <- metadata %>%
  left_join(slopes_clean %>%
              mutate(MIU_VALVE = as.numeric(MIU_VALVE)), 
            by = c("miu_valve" = "MIU_VALVE")) %>%
  rename(time2 = TIMESTAMP) %>%
  filter(year(time2) >= 2021)

write.csv(slopes_metadata, "L1.csv", row.names = FALSE)

slopes_clean %>%
  filter(year(TIMESTAMP) >= 2021) %>%
  ggplot(aes(x = TIMESTAMP, y = CH4_slope_umol_per_day)) +
  geom_point() +
  facet_wrap(~MIU_VALVE)
