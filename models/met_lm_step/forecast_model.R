# met.lm.step model
# written by ASL


#### Step 0: load packages
library(tidyverse)

#### Step 1: Set model specifications
model_id <- "met_lm_step"
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

  # Filter to desired variable, site, date
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean, 
                     by = c("datetime")) %>%
    na.omit()
  
  if(!var %in% names(site_target) || sum(!is.na(site_target[var])) == 0){
    message(paste0("No target observations at site ",site,
                   ". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$AirTemp_C_mean) & 
                !is.na(site_target$RH_percent_mean) &
                !is.na(site_target$WindSpeed_ms_mean) &
                !is.na(site_target$Rain_mm_sum) &
                !is.na(site_target[var]))<10){
    message(paste0("Insufficient met data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    # Fit linear model based on past data: target = m * air temp + b
    all <- lm(get(var) ~ AirTemp_C_mean * 
                RH_percent_mean * 
                Rain_mm_sum *
                WindSpeed_ms_mean, 
              data = site_target) #complete model
    fit <- step(all, trace=0) #trim model
    
    #  Get 30-day predicted temp ensemble at the site
    noaa_future <- noaa_future_daily 
    
    new_data <- noaa_future |>
      select(AirTemp_C_mean, RH_percent_mean, Rain_mm_sum, WindSpeed_ms_mean)
    
    preds <- predict(fit, new_data) #THIS IS THE FORECAST STEP
    
    # use the linear model to forecast target variable for each ensemble member
    forecast <- noaa_future |> 
      mutate(site_id = site,
             prediction = preds, 
             variable = var) %>%
      group_by(datetime, reference_datetime, site_id, variable) %>%
      summarise(mu = mean(prediction, na.rm = T),
                sigma = sqrt(sd(prediction, na.rm = T)^2 + sd(fit$residuals)^2),
                .groups = "drop") %>%
      pivot_longer(cols = c(mu, sigma), names_to = "parameter", values_to = "prediction")
    
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(project_id = "gcrew",
             model_id = model_id,
             reference_datetime = forecast_date,
             duration = "P1D",
             family = "normal") |>
      select(project_id, model_id, datetime, reference_datetime, duration,
             site_id, family, parameter, variable, prediction)
  }
}
