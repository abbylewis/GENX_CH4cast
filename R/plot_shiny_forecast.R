plot_shiny_forecast <- function(forecast_date, 
                                model_id,
                                hist_data){
  chamber_levels = c("c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
                     "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75", "c_10_e4.5",
                     "c_11_e5.25", "c_12_e6.0")
  if(file.exists(here::here("outputs", paste0("daily-", forecast_date, "-", model_id, ".csv.gz")))){
    forecast_file <- here::here("outputs", paste0("daily-", forecast_date, "-", model_id, ".csv.gz"))
  } else {
    message("Running forecast. Please hold :)")
    
    #Change file path here to try a different model
    source(here::here("models",model_id,"forecast_model.R"))
    
    #Run remaining code and visualize forecasts
    source(here::here("R","generate_tg_forecast.R"))
    generate_tg_forecast(forecast_date = forecast_date,
                         forecast_model = forecast_model,
                         model_variables = model_variables,
                         model_id = model_id,
                         all_sites = all_sites,
                         sites = sites,
                         noaa = noaa, 
                         plot = F,
                         save = T)
    forecast_file <- here::here("outputs", paste0("daily-", forecast_date, "-", model_id, ".csv.gz"))
  }
  
  forecast <- read_csv(forecast_file)
  target <- read_csv(here::here("L1_target.csv"), show_col_types = F)
  horiz = 35
  step = 1
  
  if(unique(forecast$family) == "ensemble"){
    p1 <- forecast %>%
      mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
      ggplot(aes(x = datetime, y = prediction)) +
      geom_hline(yintercept = 0, color = "grey50") +
      geom_vline(xintercept = forecast_date) +
      geom_line(aes(group = parameter)) +
      geom_point(data = target %>%
                   mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
                   dplyr::filter(datetime >= forecast_date - hist_data * step,
                          datetime <= forecast_date + horiz * step), 
                 aes(x = datetime, y = observation, alpha = datetime >= forecast_date), 
                 color = "red") +
      scale_alpha_manual(values = c(1, .5)) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      facet_wrap(~site_id) +
      ggtitle(paste0("Forecasts for ", forecast_date),
              paste0("Model: ", model_id))
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
                   dplyr::filter(datetime >= forecast_date - hist_data * step,
                          datetime <= forecast_date + horiz * step), 
                 aes(x = datetime, y = observation, alpha = datetime >= forecast_date)) +
      scale_alpha_manual(values = c(1, .2)) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      ylab("Flux (µmol/day)") +
      facet_wrap(~site_id) +
      ggtitle(paste0("Forecasts for ", forecast_date),
              paste0("Model: ", model_id))
  }
  
  return(p1)
}