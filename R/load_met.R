source(here::here("R","convert_to_vera_met_P1D.R"))
source(here::here("R","get_ensemble_forecast.R"))

#' load_met
#' 
#' Currently only set up for real-time forecasts (i.e., not re-analysis)
#'
#' @param forecast_date reference date for forecast generation
#' @param forecast_days days into the future to forecast
#'
#' @return no return. Exports historical and future met
#'
load_met <- function(forecast_date = Sys.Date(),
                     forecast_days = 35) {
  
  #Specify variables
  variables <- c("relativehumidity_2m", 
                 "precipitation", 
                 "windspeed_10m", 
                 "temperature_2m",
                 "shortwave_radiation")
  
  variables_renamed <- c("RH_percent_mean", 
                         "Rain_mm_sum", 
                         "WindSpeed_ms_mean", 
                         "AirTemp_C_mean",
                         "ShortwaveRadiation_Wm2")
  
  #Load site info
  lat <- 38.87477
  long <-  -76.54977
  
  #Weather predictions
  message("Loading weather predictions")
  weather_pred <- get_ensemble_forecast(
    forecast_date = forecast_date,
    latitude = lat,
    longitude = long,
    forecast_days = forecast_days, # days into the future
    past_days = 92, # past days that can be used for model fitting
    model = "gfs_seamless", # this is the NOAA gefs ensemble model
    variables = variables) |> 
    # function to convert to EFI standard
    #ropenmeteo::convert_to_efi_standard() |>
    # rename variables to match met station
    convert_to_vera_met_P1D() 

  message("Loading historical weather")
  weather_hist <- ropenmeteo::get_historical_weather(
    latitude = lat,
    longitude = long,
    start_date = as.Date("2010-01-01"),
    end_date = as.Date(forecast_date),
    variables = variables) |> 
    # function to convert to EFI standard
    #ropenmeteo::convert_to_efi_standard() |>
    # rename variables to match met station
    convert_to_vera_met_P1D() 
  
  message("Adjusting forecasts to match historical data")
  comparison_mod <- weather_hist %>%
    rename(hist_pred = prediction) %>%
    filter(!is.na(hist_pred)) %>%
    left_join(weather_pred, by = c("datetime", "variable")) %>%
    filter(!is.na(prediction)) %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by(datetime, variable) %>%
    summarize(future_sd = sd(prediction),
              future = mean(prediction),
              hist = unique(hist_pred),
              .groups = "drop")
  
  weather_pred_adjust <- weather_pred
  for(var in variables_renamed){
    if(nrow(comparison_mod %>% filter(variable == var) >= 10)){
      lm <- lm(future ~ hist, data = comparison_mod %>% filter(variable == var))
    }
    weather_pred_adjust <- weather_pred_adjust %>%
      mutate(prediction = ifelse(variable == var, 
                             prediction - lm$coefficients[1] + (1-lm$coefficients[2]) * prediction, 
                             prediction))
  }
  
  #Filter to the future
  weather_pred_export <- weather_pred_adjust %>%
    select(-unit, -site_id) %>%
    filter(datetime >= forecast_date) %>%
    pivot_wider(names_from = variable, values_from = prediction) %>%
    rename(parameter = ensemble)
  
  write.csv(weather_pred_export,
            here::here("met_downloads",
                       paste0("future_daily_",forecast_date,".csv")),
            row.names = F)
  
  write.csv(weather_hist %>%
              dplyr::select(-unit, -site_id) %>%
              pivot_wider(names_from = variable, values_from = prediction),
            here::here("met_downloads",
                       paste0("past_daily_",forecast_date,".csv")),
            row.names = F)
  return()
}
load_met(Sys.Date())
