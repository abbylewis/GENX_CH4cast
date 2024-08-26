source("./models/TBATS/forecast_model.R")
source("./R/rerun_forecasts.R")

END <- as_date('2024-03-20') #Used to fix issues if needed

rerun_forecasts(model_id, forecast_model, model_themes, END, noaa, all_sites)
