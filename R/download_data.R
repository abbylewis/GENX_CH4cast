#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
library(tidyverse)

#Identify all files
files <- drop_dir(path = "GCREW_LOGGERNET_DATA")
relevant_files <- files %>%
  filter(grepl("GENX_INSTRUMENT_FLUX_COMB", name))
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

data <- list.files("./Raw_data/dropbox_downloads", full.names = T) %>%
  map(read_csv, show_col_types = F, skip = 1) %>%
  bind_rows() %>%
  filter(!is.na(TIMESTAMP),
         !TIMESTAMP == "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  distinct() %>%
  filter(!duplicated(LGR_Time)) #I've spent some time looking into this. CH4 is also duplicated for these

p <- read_csv("./Raw_data/dropbox_downloads/GENX_INSTRUMENT_FLUX_COMB_20240501020048.dat", show_col_types = F, skip = 1)

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
filtered_data <- data %>%
  filter(!MIU_VALVE == 16) %>%
  arrange(TIMESTAMP) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE) %>%
  mutate(start = min(TIMESTAMP),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         index = row_number(),
         n = sum(index > index_cutoff)) %>%
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
  geom_point()

slopes_clean <- slopes %>%
  filter(CH4_rmse < fit_cutoff,
         CH4_init > 1,
         CH4_init < 2.5)

#R2 is not a good way to filter because this preferentially removes values near 0
slopes_clean %>%
  ggplot(aes(x = CH4_init, y = CH4_slope_umol_per_day, color = CH4_R2)) +
  geom_point() +
  geom_smooth()

#High p-values are all close to 0 (good)
slopes_clean %>%
  ggplot(aes(x = CH4_init, y = CH4_slope_umol_per_day, color = CH4_p)) +
  geom_point() +
  geom_smooth()

metadata <- read_csv("Raw_data/chamber_metadata.csv", show_col_types = F)
slopes_metadata <- metadata %>%
  left_join(slopes_clean, by = c("miu_valve" = "MIU_VALVE")) %>%
  rename(time2 = TIMESTAMP)

write.csv(slopes_metadata, "L1_2024.csv", row.names = FALSE)

slopes_clean %>%
  ggplot(aes(x = TIMESTAMP, y = CH4_slope_umol_per_day)) +
  geom_point() +
  facet_wrap(~MIU_VALVE)