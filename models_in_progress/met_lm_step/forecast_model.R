# asl.met.lm.step model
# written by ASL


#### Step 0: load packages
library(tidyverse)
#remotes::install_github("LTREB-reservoirs/vera4castHelpers")
#remotes::install_github("eco4cast/read4cast")
library(vera4castHelpers)
library(read4cast)
source("./R/download_target.R")
source("./R/load_met.R") #need to update
#library(forecast)

#### Step 1: Set model specifications
model_id <- "asl.met.lm.step"
# Currently only set up for daily variables, and not binary variables
priority_daily <- read_csv("priority_daily.csv", show_col_types = FALSE) %>%
  dplyr::filter(!grepl("binary", `"official" targets name`))
model_variables <- priority_daily$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
target_depths = "target" #Depths to forecast
noaa = T #Whether the model requires NOAA data

#### Define the forecast model for a site
forecast_model <- function(specific_depth,
                           site,
                           var,
                           noaa_past_mean,
                           noaa_future_daily,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running depth: ", specific_depth))

  # Filter to desired variable, site, date
  site_target_trimmed <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation, depth_m) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date)
  
  # Isolate target depth
  if(is.na(specific_depth)){
    site_target_raw = site_target_trimmed |> 
      filter(is.na(depth_m))
  } else {
    site_target_raw = site_target_trimmed |>
      dplyr::filter(depth_m == specific_depth)
  }
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean|>
                       filter(site_id == site), 
                     by = c("datetime", "site_id"))
  
  if(!var %in% names(site_target) || sum(!is.na(site_target[var])) == 0){
    message(paste0("No target observations at site ",site,
                   ". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$AirTemp_C_mean) & 
                !is.na(site_target$RH_percent_mean) &
                !is.na(site_target$WindSpeed_ms_mean) &
                !is.na(site_target$Rain_mm_sum) &
                !is.na(site_target[var]))==0){
    message(paste0("No complete met data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    # Fit linear model based on past data: target = m * air temp + b
    all <- lm(get(var) ~ AirTemp_C_mean * 
                RH_percent_mean * 
                Rain_mm_sum *
                WindSpeed_ms_mean, 
              data = site_target) #THIS IS THE MODEL
    
    fit <- step(all, trace=0)
    message(fit$coefficients)
    
    #  Get 30-day predicted temp ensemble at the site
    noaa_future <- noaa_future_daily %>%
      filter(site_id==site)
    
    # use the linear model to forecast target variable for each ensemble member
    forecast <- noaa_future |> 
      mutate(site_id = site,
             prediction = predict(fit, tibble(AirTemp_C_mean, 
                                              RH_percent_mean, 
                                              Rain_mm_sum, 
                                              WindSpeed_ms_mean)), #THIS IS THE FORECAST STEP
             variable = var)
    
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(project_id = "vera4cast",
             model_id = model_id,
             reference_datetime = forecast_date,
             duration = "P1D",
             depth_m = specific_depth,
             family = "ensemble") |>
      select(project_id, model_id, datetime, reference_datetime, duration, depth_m,
             site_id, family, parameter, variable, prediction)
  }
}
