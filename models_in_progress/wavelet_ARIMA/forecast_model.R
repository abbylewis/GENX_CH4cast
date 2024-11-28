# wavelet_arima model
# written by ASL


#### Step 0: load packages

library(tidyverse)
source("./models/wavelet_ARIMA/analyze_wavelets.R")
library(forecast)

#### Step 1: Set model specifications
model_id <- "wavelet_arima"
all_forecast_vars <- read_csv("forecast_variables.csv", show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data

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
  
  # Format site data for the model
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

  h = as.numeric(forecast_date - max(site_target$datetime) + horiz)
  
  #Wavelet decomposition
  dfs <- analyze_wavelets(target = site_target, site = site, var_name = var)

  # Fit arima model to each scale
  dfs_fit <- dfs %>%
    group_by(site_id, name) %>%
    nest() %>%
    summarize(fit = map(data, ~auto.arima(.x$value)),
              .groups = "drop")
  
  #Function to generate probabilistic forecasts for each scale
  pred_wavelet <- function(fit, h, npaths = 100){
    out <- as.data.frame(forecast(fit, h = h, level=0.68)) #One SD
    sim <- matrix(NA, nrow = npaths, ncol = h)
    for (i in 1:npaths){
      sim[i, ] <- simulate(fit, nsim = h, bootstrap = TRUE)
    }
    out <- as.data.frame(t(sim))
    out$datetime <- (1:h)*step+max(site_target$datetime)
    out <- out %>%
      pivot_longer(cols = paste0("V", 1:npaths), names_to = "parameter", values_to = "prediction") %>%
      mutate(parameter = as.numeric(sub("V", "", parameter)))
    return(out)
  }
  
  # Forecast
  dfs_pred <- dfs_fit %>%
    group_by(name) %>%
    mutate(forecast = map(fit, pred_wavelet, h = h)) %>%
    unnest(forecast) 
  
  #Inverse wavelet transform
  run_inverse_wavelet <- function(data){
    data <- ungroup(data) 
    J = length(unique(data$name))
    wave_recon <- list()
    for(jj in 1:J){
      wave_recon[[jj]] <- data %>%
        filter(name == unique(data$name)[jj]) %>%
        pull(prediction)
    }
    names(wave_recon) <- unique(data$name)
    class(wave_recon) <- "modwt"
    attr(wave_recon, "wavelet") <- "la8"
    attr(wave_recon, "boundary") <- "periodic"
    out <- imodwt(wave_recon)
    return(data.frame(prediction = out,
                      datetime = unique(data$datetime)))
  }
  
  forecast <- dfs_pred %>%
    group_by(parameter) %>%
    nest() %>%
    mutate(prediction = map(data, run_inverse_wavelet)) %>%
    unnest(prediction) %>%
    mutate(project_id = "gcrew",
           model_id = model_id,
           site_id = site,
           reference_datetime = forecast_date,
           duration = "P1D",
           family = "ensemble",
           variable = var)%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

