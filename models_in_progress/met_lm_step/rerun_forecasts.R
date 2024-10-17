source("./Generate_forecasts/met_lm/forecast_model.R")
source("./R/rerun_forecasts.R")
source("./R/generate_tg_forecast.R")

END <- as_date('2022-01-01') # Don't re-run if forecasts have been submitted after this date

rerun_forecasts(forecast_model = forecast_model,
                model_variables = model_variables,
                model_id = model_id,
                all_sites = all_sites,
                sites = sites,
                target_depths = target_depths,
                noaa = noaa,
                END = END)