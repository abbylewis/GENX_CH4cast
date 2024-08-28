source("./R/run_all_sites.R")
source("./R/load_met.R")

generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_variables,
                                 model_id,
                                 all_sites = all_sites, #Whether the model is /trained/ across all sites
                                 sites = sites, #Sites to forecast
                                 noaa = T,
                                 save = T,
                                 plot = F) {
  
  ### Step 1: Download latest target data
  target <- generate_target()
  
  ### Step 2: Set forecast specifications
  if(sites == "all"){
    sites <- unique(target$site_id)
  }
  horiz = 35
  step = 1
  
  ### Step 3: Get NOAA driver data (if needed)
  if(noaa){ #Some forecasts do not use any noaa driver data --> in that case skip download
    forecast_date <- as.Date(forecast_date)
    
    #This function loads meteorology and harmonizes past/future predictions
    load_met(forecast_date = forecast_date) 
    
    #Identify available files
    saved_met <- list.files(paste0("./met_downloads/"))
    saved_met_relevant <- saved_met[grepl(forecast_date, saved_met)]

    #Load forecasts
    noaa_future_daily <- read_csv(paste0("./met_downloads/",
                                         saved_met_relevant[grepl("future", saved_met_relevant)])) |> 
      mutate(datetime = lubridate::as_date(datetime))
    
    # Load historical data
    noaa_past_mean <- read_csv(paste0("./met_downloads/",
                                         saved_met_relevant[grepl("past", saved_met_relevant)])) |> 
      mutate(datetime = lubridate::as_date(datetime))
    
  } else {
    forecast_date <- as.Date(forecast_date)
    noaa_future_daily <- NULL
    noaa_past_mean <- NULL
  }
  
  ### Step 4: forecast!
  # Run all variables
  if(all_sites == F) {
    # Run all sites and depths individually for each variable
    forecast <- map_dfr(.x = model_variables,
                        .f = run_all_sites,
                        sites = sites,
                        forecast_model = forecast_model,
                        noaa_past_mean = noaa_past_mean,
                        noaa_future_daily = noaa_future_daily,
                        target = target,
                        horiz = horiz,
                        step = step,
                        forecast_date = forecast_date)
  } else {
    # Fit model across all sites together for each variable
    forecast <- map_dfr(.x = model_variables,
                        .f = forecast_model,
                        sites = sites,
                        noaa_past_mean = noaa_past_mean,
                        noaa_future_daily = noaa_future_daily,
                        target = target,
                        horiz = horiz,
                        step = step,
                        forecast_date = forecast_date)
  }
  
  forecast$model_id <- model_id
  
  ### Step 5: Format and submit
  
  # Write forecast to disk
  if(save){
    forecast_file <- paste0("outputs/daily-", forecast_date, "-", model_id, ".csv.gz")
    write_csv(forecast, forecast_file)
  }
  
  if(plot) {
    if(unique(forecast$family) == "ensemble"){
      p1 <- forecast %>%
        ggplot(aes(x = datetime, y = prediction, group = parameter)) +
        geom_line() +
        facet_grid(cols = vars(variable), rows = vars(site_id), scales = "free_y")
    } else {
      p1 <- forecast %>%
        pivot_wider(names_from = "parameter", values_from = "prediction") %>%
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = mu)) +
        geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma), alpha = 0.3) +
        facet_grid(cols = vars(variable), rows = vars(site_id), scales = "free_y")
    }
    print(p1)
  }
}
