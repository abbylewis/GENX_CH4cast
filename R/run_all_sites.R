#Quick function to repeat for all sites
run_all_sites = function(var,
                        sites,
                        forecast_model,
                        noaa_past_mean,
                        noaa_future_daily,
                        target,
                        horiz,
                        step,
                        forecast_date,
                        target_depths) {
  
  message(paste0("Running variable: ", var))
  forecast <- map_dfr(.x = sites,
                      .f = forecast_model,
                      forecast_model = forecast_model,
                      noaa_past_mean = noaa_past_mean,
                      noaa_future_daily = noaa_future_daily,
                      var = var,
                      target = target,
                      horiz = horiz,
                      step = step,
                      forecast_date = forecast_date,
                      target_depths = target_depths)
  
}
