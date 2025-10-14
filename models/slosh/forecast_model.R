# slosh model
# written by ASL


#### Step 0: load packages
library(tidyverse)

#### Step 1: Set model specifications
model_id <- "slosh"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = T #Whether the model requires NOAA data

#### Define the forecast model for a site
forecast_model <- function(site,
                           var,
                           noaa_past_mean,
                           noaa_future_daily,
                           target,
                           horiz,
                           step,
                           forecast_date) {
  
  message(paste0("Running site: ", site))
  
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
           !is.na(roll_temp),
           !is.na(ch4_lag1),
           !is.na(WaterLevel),
           !is.na(WindSpeed_ms_mean),
           !is.na(CH4_slope_umol_m2_day))
  
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
  start <- max(as.Date(new_data$datetime)[!is.na(new_data$CH4_slope_umol_m2_day)], na.rm=T) + days(1)
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
    rename(prediction = CH4_slope_umol_m2_day)  %>%
    mutate(project_id = "gcrew",
           model_id = model_id,
           site_id = site,
           reference_datetime = forecast_date,
           duration = "P1D",
           family = "normal",
           variable = var) %>%
    group_by(datetime, reference_datetime, site_id, variable, project_id, 
             model_id, duration, family) %>%
    summarise(mu = mean(prediction, na.rm = T),
              sigma = sqrt(sd(prediction, na.rm = T)^2 + #from ensemble
                             sd(site_target_rf$rf_resid, na.rm = T)^2),
              .groups = "drop") %>%
    pivot_longer(cols = c(mu, sigma), names_to = "parameter", values_to = "prediction") %>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction) 
}
