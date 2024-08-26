
#Change file path here:
source("./models/ARIMA/forecast_model.R")

#Run remaining code and visualize plot
source("./R/generate_tg_forecast.R")
tryCatch({
  generate_tg_forecast(forecast_date = Sys.Date(),
                       forecast_model = forecast_model,
                       model_variables = model_variables,
                       model_id = model_id,
                       all_sites = all_sites,
                       sites = sites,
                       noaa = noaa, 
                       plot = T,
                       save = T)
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})