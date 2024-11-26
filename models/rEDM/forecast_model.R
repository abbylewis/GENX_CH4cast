# rEDM model
# written by ASL


#### Step 0: load packages
library(tidyverse)
source(here::here("R","generate_target.R"))
library(rEDM)
library(rjags)
library(coda)

#### Step 1: Set model specifications
model_id <- "rEDM"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
message(paste0("Running EDM. Sites are ", sites))

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
  
  # Format site data for rEDM model
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
  
  # Fit rEDM model
  site_target = site_target_raw |>
    complete(datetime = full_seq(datetime, 1), site_id) %>%
    #filter(!(datetime > "2022-10-13" & datetime <= "2023-06-27")) %>%
    arrange(datetime)
  
  n <- nrow(site_target)
  h <- (forecast_date-max(site_target$datetime))+horiz
  lib <- c(1, n) # use all data for training (why not)
  
  dims <- EmbedDimension(dataFrame = data.frame(site_target %>% select(-site_id)),
                         columns = "CH4_slope_umol_per_day", target = "CH4_slope_umol_per_day", 
                         lib = lib, pred = lib,  # which portions of the data to train and predict
                         maxE = 5)
  opt_dim <- which.max(dims$rho)
  
  fits <- rEDM::SMap(dataFrame = data.frame(site_target %>% select(-site_id)),
                       columns = "CH4_slope_umol_per_day", target = "CH4_slope_umol_per_day", 
                       lib = lib, pred = lib, # which portions of the data to train and predict
                       E = opt_dim)$predictions
  
  fits %>%
    ggplot(aes(x = datetime)) +
    geom_point(aes(y = Observations), color = "blue") +
    geom_line(aes(y = Predictions), color = "red")
  
  output <- rEDM::SMap(dataFrame = data.frame(site_target %>% select(-site_id)),
                          columns = "CH4_slope_umol_per_day", target = "CH4_slope_umol_per_day", 
                          lib = lib, pred = lib, # which portions of the data to train and predict
                          E = opt_dim, generateSteps = h)$predictions
  
  #generateSteps will override prediction interval
  #See more here: https://sugiharalab.github.io/EDM_Documentation/simplex_/

  forecast = data.frame(project_id = "gcrew",
                        model_id = model_id,
                        datetime = output$datetime,
                        reference_datetime = forecast_date,
                        duration = "P1D",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = output$Predictions,
                        sigma = sqrt(output$Pred_Variance)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

