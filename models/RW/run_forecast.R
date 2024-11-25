source("./models/RW/forecast_model.R")
source("./R/generate_tg_forecast.R")

tryCatch({
  generate_tg_forecast(forecast_date = Sys.Date(),
                       forecast_model = forecast_model,
                       model_variables = model_variables,
                       model_id = model_id,
                       all_sites = all_sites,
                       sites = sites,
                       noaa = noaa, 
                       plot = F,
                       save = T)
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})

tryCatch({
  generate_tg_forecast(forecast_date = Sys.Date(),
                       forecast_model = forecast_model,
                       model_variables = model_variables,
                       model_id = paste0(model_id,"_no2021"),
                       all_sites = all_sites,
                       sites = sites,
                       noaa = noaa, 
                       plot = F,
                       save = T,
                       use_ref_year = F)
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})

#For troubleshooting
#forecast_date = Sys.Date()
