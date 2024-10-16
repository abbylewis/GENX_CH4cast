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
  target <- read_csv("L1_target.csv", show_col_types = F)
  
  ### Step 2: Set forecast specifications
  if(sites == "all"){
    sites <- unique(target$site_id)
  }
  horiz = 35
  step = 1
  chamber_levels = c("c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
                     "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75", "c_10_e4.5",
                     "c_11_e5.25", "c_12_e6.0")
  
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
    forecast <- purrr::pmap(.l = list(model_variables),
                            .f = run_all_sites,
                            sites = sites,
                            forecast_model = forecast_model,
                            noaa_past_mean = noaa_past_mean,
                            noaa_future_daily = noaa_future_daily,
                            target = target,
                            horiz = horiz,
                            step = step,
                            forecast_date = forecast_date) %>%
      bind_rows()
  } else {
    # Fit model across all sites together for each variable
    forecast <- purrr::pmap(.l = list(model_variables),
                            .f = forecast_model,
                            sites = sites,
                            noaa_past_mean = noaa_past_mean,
                            noaa_future_daily = noaa_future_daily,
                            target = target,
                            horiz = horiz,
                            step = step,
                            forecast_date = forecast_date) %>%
      bind_rows()
  }
  
  if(nrow(forecast) == 0){
    stop("No forecast generated")
  }
  
  forecast <- forecast %>%
    mutate(model_id = model_id) %>%
    filter(datetime >= forecast_date)
  
  ### Step 5: Format and submit
  
  # Write forecast to disk
  if(save){
    forecast_file <- paste0("outputs/daily-", forecast_date, "-", model_id, ".csv.gz")
    write_csv(forecast, forecast_file)
  }
  
  if(plot) {
    if(unique(forecast$family) == "ensemble"){
      p1 <- forecast %>%
        mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
        ggplot(aes(x = datetime, y = prediction)) +
        geom_hline(yintercept = 0, color = "grey50") +
        geom_vline(xintercept = forecast_date) +
        geom_line(aes(group = parameter)) +
        geom_point(data = target %>%
                     mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
                     filter(datetime >= forecast_date - 35 * step,
                            datetime <= forecast_date + horiz * step), 
                   aes(x = datetime, y = observation, alpha = datetime >= forecast_date), 
                   color = "red") +
        scale_alpha_manual(values = c(1, .5)) +
        theme(legend.position = "none")
      if(length(model_variables) == 1) {
        p1 <- p1 +
          facet_wrap(~site_id)
      } else {
        p1 <- p1 +
          facet_grid(rows = vars(variable), cols = vars(site_id))
      }
    } else {
      p1 <- forecast %>%
        mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
        pivot_wider(names_from = "parameter", values_from = "prediction") %>%
        ggplot(aes(x = datetime)) +
        geom_hline(yintercept = 0, color = "grey50") +
        geom_vline(xintercept = forecast_date) +
        geom_line(aes(y = mu)) +
        geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma), alpha = 0.3) +
        geom_point(data = target %>%
                     mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
                     filter(datetime >= forecast_date - 35 * step,
                            datetime <= forecast_date + horiz * step), 
                   aes(x = datetime, y = observation, alpha = datetime >= forecast_date)) +
        scale_alpha_manual(values = c(1, .5)) +
        theme(legend.position = "none") +
        ylab("Flux (µmol/day)")
      if(length(model_variables) == 1) {
        p1 <- p1 +
          facet_wrap(~site_id)
      } else {
        p1 <- p1 +
          facet_grid(rows = vars(variable), cols = vars(site_id))
      }
    }
    print(p1)
  }
}
