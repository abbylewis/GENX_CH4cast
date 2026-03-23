source("./models/slosh/forecast_model.R")
source("./R/rerun_forecasts.R")
source("./R/generate_tg_forecast.R")

END <- as_date('2026-03-23') # Don't re-run if forecasts have been submitted after this date

rerun_forecasts(forecast_model = forecast_model,
                model_variables = model_variables,
                model_id = model_id,
                all_sites = all_sites,
                sites = sites,
                noaa = noaa,
                END = END,
                start_date = '2025-05-01',
                end_date = Sys.Date())
