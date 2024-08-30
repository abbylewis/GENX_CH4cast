#### Load packages
source("./R/load_met.R")
source("./R/generate_target.R")
library(tidyverse)
source("./R/generate_target.R")
library(forecast)

### Set model specifications
model_id <- "test"
all_forecast_vars <- read_csv("forecast_variables.csv", show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
boot_number = 100 #Number of bootstraps to run
bootstrap = F

### Download latest target data
target <- generate_target()

### Set forecast specifications
if(sites == "all"){
  sites <- unique(target$site_id)
}
horiz = 35
step = 1
#For testing
site <- sites[1]
var <- model_variables[1]
forecast_date <- as.Date("2023-07-01")

### Get NOAA driver data (if needed)
if(noaa){ #Some forecasts do not use any noaa driver data --> in that case skip download
  forecast_date <- as.Date(forecast_date)
  
  #This function loads meteorology and harmonizes past/future predictions
  load_met(forecast_date = forecast_date) 
  
  #Identify available files
  saved_met <- list.files(paste0("./met_downloads/"))
  saved_met_relevant <- saved_met[grepl(forecast_date, saved_met)]
  
  #Load forecasts
  noaa_future_daily <- read_csv(paste0("./met_downloads/",
                                       saved_met_relevant[grepl("future", saved_met_relevant)])) |> 
    mutate(datetime = lubridate::as_date(datetime))
  
  # Load historical data
  noaa_past_mean <- read_csv(paste0("./met_downloads/",
                                    saved_met_relevant[grepl("past", saved_met_relevant)])) |> 
    mutate(datetime = lubridate::as_date(datetime))
  
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


