# ridge model
# The only difference between this and lasso is the alpha value
# written by ASL


#### Step 0: load packages
library(tidyverse)
library(glmnet)

#### Step 1: Set model specifications
model_id <- "ridge"
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
                !is.na(site_target$Pressure_Pa) &
                !is.na(site_target[var]))<10){
    message(paste0("Insufficient met data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    data_matrix <- site_target |>
      dplyr::select(AirTemp_C_mean, RH_percent_mean, Rain_mm_sum, WindSpeed_ms_mean, Pressure_Pa) %>%
      as.matrix()
    y_var <- site_target[[var]]
    #perform k-fold cross-validation to find optimal lambda value
    cv_model <- cv.glmnet(data_matrix, y_var, alpha = 0)
    best_lambda <- cv_model$lambda.min
    
    #Fit model
    fit <- glmnet::glmnet(y = y_var,
                          x = data_matrix,
                          alpha = 0, 
                          lambda = best_lambda)
    fit$predicted <- predict(fit, s = best_lambda, newx = data_matrix)
    
    #  Get 30-day predicted temp ensemble at the site
    new_data <- noaa_future_daily |>
      select(AirTemp_C_mean, RH_percent_mean, Rain_mm_sum, WindSpeed_ms_mean, Pressure_Pa) %>%
      as.matrix()
    
    preds <- predict(fit, new_data, s = best_lambda) #THIS IS THE FORECAST STEP
    
    # use model to forecast target variable for each ensemble member
    forecast <- noaa_future_daily |> 
      mutate(site_id = site,
             prediction = preds, 
             variable = var) %>%
      group_by(datetime, reference_datetime, site_id, variable) %>%
      summarise(mu = mean(prediction, na.rm = T),
                sigma = sqrt(sd(prediction, na.rm = T)^2 + 
                               sd(fit$predicted - site_target[[var]], na.rm = T)^2),
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
