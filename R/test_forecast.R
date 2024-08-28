
#Change file path here to try a different model
source("./models/climatology/forecast_model.R")

#Run remaining code and visualize forecasts
source("./R/generate_tg_forecast.R")
generate_tg_forecast(forecast_date = as.Date("2024-07-15"),
                     forecast_model = forecast_model,
                     model_variables = model_variables,
                     model_id = model_id,
                     all_sites = all_sites,
                     sites = sites,
                     noaa = noaa, 
                     plot = T,
                     save = F)
