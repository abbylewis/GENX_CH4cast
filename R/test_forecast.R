
#Load new target data (optional)
source("./R/generate_target.R")
#generate_target()

#Change file path here to try a different model
source("./models/prophet/forecast_model.R")

#Run remaining code and visualize forecasts
source("./R/generate_tg_forecast.R")
generate_tg_forecast(forecast_date = as.Date("2024-10-16"),
                     forecast_model = forecast_model,
                     model_variables = model_variables,
                     model_id = model_id,
                     all_sites = all_sites,
                     sites = sites,
                     noaa = noaa, 
                     plot = T,
                     save = F)

