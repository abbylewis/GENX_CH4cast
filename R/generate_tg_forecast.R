source("./R/run_all_sites.R")
source("./R/load_met.R")

generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_variables,
                                 model_id,
                                 all_sites = all_sites, #Whether the model is /trained/ across all sites
                                 sites = sites, #Sites to forecast
                                 noaa = T,
                                 target_depths = target_depths,
                                 first_submission = FALSE) {
  
  ### Step 1: Set forecast specifications
  if(sites == "all"){
    sites <- c("a0_amb", "c1_amb", "b2_e0.75", "a3_e1.5", "b4_e2.25", "c4_e2.25",
               "a5_e3.0", "b6_e3.75", "c6_e3.75", "a7_e4.5", "c8_e5.25", "b9_e6.0")
  }
  
  ### Step 2: Get NOAA driver data (if needed)
  if(noaa){ #Some forecasts do not use any noaa driver data --> in that case skip download
    forecast_date <- as.Date(forecast_date)
    
    #This function loads meteorology and harmonizes past/future predictions
    map(sites, load_met, forecast_date = forecast_date) 
    
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
  
  ### Step 3: Download latest target data
  target <- generate_target()
  
  ### Step 4: forecast!
  
  ## Test with a single site/variable/depth first!
  #forecast <- map_dfr(.x = model_variables[1],
  #                    .f = run_all_sites,
  #                    sites = sites[1],
  #                    forecast_model = forecast_model,
  #                    noaa_past_mean = noaa_past_mean,
  #                    noaa_future_daily = noaa_future_daily,
  #                    target = target,
  #                    horiz = horiz,
  #                    step = step,
  #                    theme = theme,
  #                    forecast_date = forecast_date,
  #                    target_depths = target_depths[1])
  
  #Visualize the ensemble predictions -- what do you think?
  #forecast |> 
  #  ggplot(aes(x = datetime, y = prediction, color = parameter)) +
  #  geom_line(alpha=0.3) +
  #  facet_wrap(~variable, scales = "free")
  
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
                        forecast_date = forecast_date,
                        target_depths = target_depths)
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
                        forecast_date = forecast_date,
                        target_depths = target_depths)
  }
  
  forecast$model_id <- model_id
  
  ### Step 5: Format and submit
  
  # Write forecast to disk
  forecast_file <- paste0("daily-", forecast_date, "-", model_id, ".csv.gz")
  write_csv(forecast_comb, forecast_file)
  
  #Visualize
  #forecast %>%
  #  pivot_wider(names_from = "parameter", values_from = "prediction") %>%
  #  mutate(site_depth = paste0(site_id, "_", depth_m)) %>%
  #  ggplot(aes(x = datetime)) +
  #  geom_line(aes(y = mu)) +
  #  geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma), alpha = 0.3) +
  #  facet_grid(rows = vars(variable), cols = vars(site_id), scales = "free_y")
  #
  #forecast |> 
  #  mutate(site_depth = paste0(site_id, "_", depth_m)) %>%
  #  ggplot(aes(x = datetime, y = prediction, color = parameter)) +
  #  geom_line(alpha=0.3) +
  #  facet_grid(rows = vars(variable), cols = vars(site_depth), scales = "free_y")
}
