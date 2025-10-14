#### Load packages
source(here::here("R","load_hist_weather.R"))
source(here::here("R","load_and_save_gefs.R"))
source(here::here("R","load_and_save_etss.R"))
source("./R/generate_target.R")
library(tidyverse)
library(forecast)
library(randomForest)

### Set model specifications
model_id <- "test"
all_forecast_vars <- read_csv("forecast_variables.csv", show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = T #Whether the model requires NOAA data
boot_number = 100 #Number of bootstraps to run
bootstrap = F
window = 10 #Number of days to use in the climate window

### Download latest target data
target <- generate_target()

### Set forecast specifications
if(sites == "all"){
  sites <- unique(target$site_id)
}
horiz = 8
step = 1
#For testing
site <- "1"
var <- model_variables[1]
forecast_date <- as.Date("2025-10-06")

### Get NOAA driver data (if needed)
if(noaa){ #Some forecasts do not use any noaa driver data --> in that case skip download
  forecast_date <- as.Date(forecast_date)
  
  #Identify available files
  saved_met <- list.files(here::here("met_downloads"))
  if(sum(grepl(forecast_date, saved_met)) == 0){
    #This function loads meteorology and harmonizes past/future predictions
    met <- load_and_save_gefs(date = forecast_date) 
    past <- load_hist_weather() #refresh historical data
  }
  
  saved_wl <- list.files(here::here("wl_forecasts"))
  date_wl <- format(forecast_date, "%Y%m%d")
  if(sum(grepl(date_wl, saved_wl)) == 0){
    wl <- load_and_save_etss(date = forecast_date) 
    past_wl <- load_hist_etss()
  }
  
  #Load forecasts
  future_wl <- read_csv(here::here("wl_forecasts",
                                   paste0("etss_future_daily_",date_wl,".csv")),
                        show_col_types = F)
  
  noaa_future_daily <- read_csv(here::here("met_downloads",
                                           paste0("future_daily_",forecast_date,".csv")),
                                show_col_types = F) %>%
    pivot_wider(names_from = "variable", values_from = "prediction") %>%
    left_join(future_wl %>% 
                rename(WaterLevel = prediction) %>%
                select(datetime, WaterLevel))
  
  # Load historical data
  past_wl <- read_csv(here::here("wl_forecasts",
                                 "past_daily_current.csv"),
                      show_col_types = F) %>%
    filter(datetime < forecast_date)
  
  noaa_past_mean <- read_csv(here::here("met_downloads",
                                        "past_daily_current.csv"),
                             show_col_types = F) %>%
    filter(datetime <= forecast_date) %>%
    pivot_wider(names_from = "variable", values_from = "prediction") %>%
    left_join(past_wl %>% 
                rename(WaterLevel = prediction) %>%
                select(datetime, WaterLevel),
              by = "datetime")
  
} else {
  forecast_date <- as.Date(forecast_date)
  noaa_future_daily <- NULL
  noaa_past_mean <- NULL
}

# Format site data
site_target_raw <- target |>
  dplyr::mutate(datetime = as.Date(datetime)) |>
  dplyr::select(datetime, site_id, variable, observation) |>
  dplyr::filter(variable == var, 
                site_id == site,
                datetime < forecast_date) |>
  tidyr::pivot_wider(names_from = "variable", values_from = "observation")

if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) == 0){
  message(paste0("No target observations at site ", site, 
                 ". Skipping forecasts at this site."))
  return()
}

site_target = site_target_raw |>
  complete(datetime = full_seq(datetime, 1), site_id)

h = as.numeric(forecast_date - max(site_target$datetime)+horiz)




### MODIFY

site_target_met <- site_target |>
  dplyr::left_join(noaa_past_mean %>%
                     select(-site_id)) %>%
  arrange(datetime) %>%
  mutate(WL_firstdif = WaterLevel - lag(WaterLevel),
         roll_temp = zoo::rollmean(AirTemp_C_mean, 5, align = "right", fill = NA),
         ch4_lag1 = lag(CH4_slope_umol_m2_day),
         WL_lag1 = lag(WaterLevel),
         roll_WL = zoo::rollmean(WaterLevel, 3, align = "right", fill = NA))

site_target_rf <- site_target_met %>%
  filter(!is.na(WL_firstdif),
         !is.na(roll_temp))

rf <- randomForest(CH4_slope_umol_m2_day ~ ch4_lag1 + WL_firstdif + roll_temp + 
                     WaterLevel + WindSpeed_ms_mean + WL_lag1, 
                   data = site_target_rf)
site_target_rf$rf_pred <- predict(rf, newdata = site_target_rf)
importance(rf)
site_target_rf %>%
  ggplot(aes(x = datetime, y = rf_pred))+
  geom_line()+
  geom_line(aes(y = CH4_slope_umol_m2_day), color = "red")

site_target_rf <-  site_target_rf %>%
  mutate(rf_resid = CH4_slope_umol_m2_day-rf_pred)
site_target_rf %>%
  ggplot(aes(y = rf_resid, x = WaterLevel, color = roll_WL))+
  geom_point()+
  geom_smooth(method = "lm")

new_data <- noaa_past_mean %>%
  crossing(parameter = 0:30) %>%
  filter(datetime < as.Date(forecast_date)) %>%
  bind_rows(noaa_future_daily %>% 
              mutate(parameter = as.numeric(parameter))) %>%
  ungroup() %>%
  group_by(parameter) %>%
  arrange(datetime) %>%
  mutate(WL_firstdif = WaterLevel - lag(WaterLevel),
         roll_temp = zoo::rollmean(AirTemp_C_mean, 5, align = "right", fill = NA),
         WL_lag1 = lag(WaterLevel),
         roll_WL = zoo::rollmean(WaterLevel, 3, align = "right", fill = NA)) %>%
  filter(!is.na(WaterLevel)) %>%
  left_join(site_target_rf %>% select(datetime, CH4_slope_umol_m2_day))

#THIS IS THE FORECAST STEP
start <- max(new_data$datetime[!is.na(new_data$CH4_slope_umol_m2_day)], na.rm=T) + days(1)
end <- as.Date(forecast_date) + days(horiz)
for(day in start:end){
  data_used <- new_data %>%
    group_by(parameter) %>%
    mutate(ch4_lag1 = lag(CH4_slope_umol_m2_day)) %>%
    filter(datetime == as.Date(day))
  
  data_used$CH4_slope_umol_m2_day <- predict(rf, newdata = data_used)
  new_data <- new_data %>%
    filter(!datetime == as.Date(day)) %>%
    bind_rows(data_used)
}

forecast <- new_data %>%
  filter(datetime >= as.Date(start)) %>%
  rename(prediction = CH4_slope_umol_m2_day) %>%
  mutate(project_id = "gcrew",
         model_id = model_id,
         site_id = site,
         reference_datetime = forecast_date,
         duration = "P1D",
         family = "ensemble",
         variable = var)%>%
  select(project_id, model_id, datetime, reference_datetime, duration,
         site_id, family, parameter, variable, prediction)
