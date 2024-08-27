
rerun_forecasts <- function(forecast_model = forecast_model,
                            model_variables = model_variables,
                            model_id = model_id,
                            all_sites = all_sites,
                            sites = sites,
                            target_depths = target_depths,
                            noaa = noaa,
                            END) {
  ### Some code to fill in missing forecasts
  # Dates of forecasts 
  # end_date <- paste(Sys.Date() - days(2), '00:00:00') #Yesterday's forecasts might not have been processed. Wait to redo
  end_date <- "2022-12-31"
  
  # Get all the submissions 
  submissions <- list.files("outputs", full.names = TRUE)
  
  # for each date, check if we have a forecast
  required_forecasts <- data.frame(date = as.character(paste0(seq.Date(as_date('2021-01-01'), 
                                                                       to = as_date(end_date), 
                                                                       by = 'day'), ' 00:00:00'))
  )
  
  for (i in 1:nrow(required_forecasts)) {
    forecast_file <- paste0("daily-", 
                            as_date(required_forecasts$date[i]), 
                            '-', model_id, 
                            '.csv.gz')
    required_forecasts[i, "daily"] <- sum(grepl(forecast_file, submissions)) > 0
    
    if (required_forecasts[i,"daily"]) {
      all_modified <- submissions[grepl(forecast_file, submissions)]
      #Recently modified?
      modified <- max(as_datetime(file.info(all_modified)$mtime))
      required_forecasts[i,"daily"] <- ifelse(modified >= END, T, F)
    }
    }
  
  # which dates do you need to generate forecasts for?
  missed_dates <- required_forecasts %>%
    filter(!daily)
  
  for (i in 1:nrow(missed_dates)) {
    
    forecast_date <- as.Date(missed_dates$date[[i]])
    
    message(paste0("Running forecasts for: ", forecast_date))
    # Generate the forecasts
    tryCatch({
      generate_tg_forecast(forecast_date = forecast_date,
                           forecast_model = forecast_model,
                           model_variables = model_variables,
                           model_id = model_id,
                           all_sites = all_sites,
                           sites = sites,
                           target_depths = target_depths,
                           noaa = noaa)
    }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
  }
}
