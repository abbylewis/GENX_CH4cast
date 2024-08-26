source("./models/temp_lm/forecast_model.R")
source("./R/generate_tg_forecast.R")

tryCatch({
  generate_tg_forecast(forecast_date = Sys.Date(),
                       forecast_model = forecast_model,
                       model_variables = model_variables,
                       model_id = model_id,
                       all_sites = all_sites,
                       sites = sites,
                       target_depths = target_depths,
                       noaa = noaa,
                       first_submission = F)
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})

#For troubleshooting
#forecast_date = Sys.Date()
