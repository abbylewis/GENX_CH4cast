
library(tidyverse)
library(sf)
source(here::here("R", "megacube_extract.R"))
source(here::here("R", "gefs-methods.R"))

#remember you need to deal with time zones

load_and_save_gefs <- function(date){
  message("About to use sf package")
  sites <- sf::st_as_sf(read.csv(here::here("Raw_data","site_gefs.csv")),
                                coords=c("longitude", "latitude"),
                                crs = 4326) |>
    tibble::rowid_to_column("FID") |>
    sf::st_transform(crs = sf::st_crs(grib_wkt()))
  message("Used sf successfully")
  
  raw <- megacube_extract(dates = date,
                        ensemble = gefs_ensemble(),
                        bands = gefs_bands(),
                        sites = sites,
                        horizon = gefs_horizon(),
                        all_bands = gefs_all_bands(),
                        url_builder = gefs_urls,
                        cycles =  c("00"))
  
  formatted <- raw %>%
    mutate(model_id = "noaa_gefs",
           parameter = substr(ensemble, 4,5)) |>
    pivot_wider(names_from = "variable", values_from = "prediction") %>%
    rename(AirTemp_C_mean = TMP,
           RH_percent_mean = RH, 
           Rain_mm_sum = APCP, 
           ShortwaveRadiation_Wm2 = DSWRF) %>%
    mutate(WindSpeed_ms_mean = sqrt(UGRD^2+VGRD^2)) %>%
    select(-UGRD, -VGRD, -horizon) |>
    pivot_longer(cols = c("AirTemp_C_mean", "RH_percent_mean", "Rain_mm_sum", "ShortwaveRadiation_Wm2", "WindSpeed_ms_mean"),
                 names_to = "variable",
                 values_to = "prediction") %>%
    mutate(datetime = as.Date(datetime, tz = "America/New_York"),
           horizon = as.numeric(difftime(datetime,
                                         reference_datetime, 
                                         units = "days"))) %>%
    group_by_at(colnames(.)[colnames(.) != "prediction"]) %>%
    summarise(sum_pred = sum(prediction),
              prediction = mean(prediction, na.rm = T),
              .groups = "drop") %>%
    mutate(prediction = ifelse(variable == "Rain_mm_sum", sum_pred, prediction)) %>%
    select(-sum_pred) %>%
    filter(horizon > 0) #don't have forecasts for all of the first day
  
  for(date_i in unique(date)){
    date_i <- as.Date(date_i)
    this_day <- formatted %>%
      filter(reference_datetime == date_i)
    date_formatted <- format(date_i, format = "%Y-%m-%d")
    write_csv(this_day, here::here("met_downloads", 
                                   paste0("future_daily_", date_formatted, ".csv")))
  }
}

#target <- read_csv(here::here("L1_target.csv"), show_col_types = F)
#date <- seq(min(target$datetime), Sys.Date() - 1L, by = "1 day")
#processed <- as.Date(str_extract(list.files(here::here("met_downloads")), "[0-9].*[0-9]"))
#date <- date[!date %in% processed]
##Process all, breaking into chucks to account for system limitations
#comb <- date %>%
#  split(cut(date, 1, labels = FALSE)) %>%
#  map(load_and_save_gefs) %>%
#  bind_rows()
