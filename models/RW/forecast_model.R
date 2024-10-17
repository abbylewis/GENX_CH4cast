# RW model
# written by ASL


#### Step 0: load packages

library(tidyverse)
source(here::here("R","generate_target.R"))
library(forecast)

#### Step 1: Set model specifications
model_id <- "RW"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
boot_number = 100 #Number of bootstraps to run
bootstrap = F

#### Step 2: Define the forecast model
forecast_model <- function(site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_daily = NULL,
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
                  datetime < forecast_date) 
  
  # Format
  site_target_raw <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) == 0){
    message(paste0("No target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }

  site_target = site_target_raw |>
    complete(datetime = full_seq(datetime, 1), site_id)

  h = as.numeric(forecast_date - max(site_target$datetime)+horiz)

  # Fit RW model
  RW_model <- site_target %>%
    mutate(var_unnamed = get(!!var)) %>%
    tsibble::as_tsibble(index = datetime, key = "site_id") %>%
    fabletools::model(RW = fable::RW(var_unnamed))
  
  if (bootstrap == T) {
    forecast <- RW_model %>%
      fabletools::generate(h = h,
                           bootstrap = T,
                           times = boot_number) |>
      rename(parameter = .rep,
             prediction = .sim) |>
      mutate(model_id = model_id,
             family = 'ensemble',
             reference_datetime = forecast_date,
             variable = var,
             project_id = "gcrew",
             duration = "P1D")  |>
      select(any_of(c("model_id", "datetime", "reference_datetime","site_id", "variable", "family",
                      "parameter", "prediction", "project_id", "duration")))|>
      select(-any_of('.model'))|>
      filter(datetime > reference_datetime)|>
      ungroup() |>
      as_tibble()
    
  }  else {
      # don't use bootstrapping
      forecast <- RW_model %>% fabletools::forecast(h = h)
      
      # extract parameters
      parameters <- distributional::parameters(forecast$var_unnamed)
      
      # make right format
      forecast <- bind_cols(forecast, parameters) |>
        pivot_longer(mu:sigma,
                     names_to = 'parameter',
                     values_to = 'prediction') |>
        mutate(model_id = model_id,
               family = 'normal',
               reference_datetime=forecast_date,
               variable = var,
               project_id = "gcrew",
               duration = "P1D") |>
        select(any_of(c("project_id", "model_id", "datetime", "reference_datetime",
                        "duration", "site_id", "family", "parameter", 
                        "variable", "prediction"))) |>
        select(-any_of('.model')) |>
        filter(datetime > reference_datetime) |>
        ungroup() |>
        as_tibble()
    }
  
  return(forecast)
}

