
sapply(list.files(here::here("R", "gefs4cast-main", "R"), full.names = T), source)
library(tidyverse)
target <- read_csv(here::here("L1_target.csv"), show_col_types = F)

dates <- seq(min(target$datetime), Sys.Date() - 1L, by = "1 day")

#remember you need to deal with time zones

load_and_save_gefs <- function(date){
  raw <- megacube_extract(dates = date,
                        ensemble = gefs_ensemble(),
                        bands = gefs_bands(),
                        sites = neon_sites(),
                        horizon = gefs_horizon(),
                        all_bands = gefs_all_bands(),
                        url_builder = gefs_urls,
                        cycles =  c("00"))
  
  formatted <- raw %>%
    mutate(model_id = "noaa_gefs",
           parameter = substr(ensemble, 4,5)) %>%
    pivot_wider(names_from = "variable", values_from = "prediction") %>%
    rename(AirTemp_C_mean = TMP,
           RH_percent_mean = RH, 
           Rain_mm_sum = APCP, 
           ShortwaveRadiation_Wm2 = DSWRF) %>%
    mutate(WindSpeed_ms_mean = sqrt(UGRD^2+VGRD^2)) %>%
    select(all_of(c("datetime", "reference_datetime", "model_id",
                  "parameter", "AirTemp_C_mean", "RH_percent_mean",
                  "Rain_mm_sum", "ShortwaveRadiation_Wm2", "WindSpeed_ms_mean")))
  
  write_csv(formatted, here::here("met_downloads", paste0("future_daily_", date, ".csv")))

}

tictoc::tic()
p <- dates[length(dates)] %>%
  map(load_and_save_gefs)
tictoc::toc()
