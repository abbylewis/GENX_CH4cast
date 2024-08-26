# asl.tbats model
# written by ASL, 21 Jan 2023



#### Step 0: load packages

library(tidyverse)
#remotes::install_github("LTREB-reservoirs/vera4castHelpers")
library(vera4castHelpers)
#remotes::install_github("eco4cast/read4cast")
library(read4cast)
source("./R/download_target.R")
library(forecast)

#### Step 1: Set model specifications
model_id <- "asl.tbats"
# Currently only set up for daily variables
# TBATS does not work for binary variables
priority_daily <- read_csv("priority_daily.csv", show_col_types = FALSE) %>%
  dplyr::filter(!grepl("binary", `"official" targets name`))
model_variables <- priority_daily$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
target_depths = "target" #Depths to forecast
noaa = F #Whether the model requires NOAA data

#### Step 2: Define the forecast model
forecast_model <- function(specific_depth,
                           site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_daily = NULL,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running depth: ", specific_depth))
  
  # Format site data for model
  site_target_trimmed <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation, depth_m) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Isolate target depth
  if(is.na(specific_depth)){
    site_target_raw = site_target_trimmed %>% filter(is.na(depth_m))
  } else {
    site_target_raw = site_target_trimmed |>
      dplyr::filter(depth_m == specific_depth)
  }
  
  # Format site data for model
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
  
  ts_data = as.ts(site_target[var])
  
  #If all data are positive, apply the correct transformation
  if(sum(ts_data < 0, na.rm=T) == 0){ #if there are no negative values, consider transformation
    #Deal with 0s before transformation
    ts_data[ts_data == 0] <- 
      min(ts_data[!is.na(ts_data) & ts_data > 0], na.rm = T)/2 
    ts_data_interp = na.interp(ts_data, lambda = "auto")
  } else {
    ts_data_interp = na.interp(ts_data)
  }
  
  # Fit tbats with interpolated data
  fit = tbats(ts_data_interp)
  
  # use the model to forecast target variable
  forecast_raw <- as.data.frame(forecast(fit,h=h,level=0.68))%>% #One SD
    mutate(sigma = `Hi 68`-`Point Forecast`)
  
  forecast = data.frame(project_id = "vera4cast",
                        model_id = model_id,
                        datetime = (1:h)*step+max(site_target$datetime),
                        reference_datetime = forecast_date,
                        duration = "P1D",
                        depth_m = specific_depth,
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(forecast_raw$`Point Forecast`),
                        sigma = as.numeric(forecast_raw$sigma)
  ) %>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration, depth_m,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}
