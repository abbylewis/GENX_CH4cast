plot_todays_forecast <- function(forecast_date, 
                                model_id,
                                hist_data){
  chamber_labels = c("c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
                     "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75", "c_10_e4.5",
                     "c_11_e5.25", "c_12_e6.0")
  chamber_levels = as.character(1:12)
  
  forecast_file <- paste0("daily-", forecast_date, "-", model_id, ".csv.gz")
  forecast <- read_csv(paste0("dashboard lightweight/data/", forecast_file), show_col_types = F)
  target <- read_csv("dashboard lightweight/data/L1_target.csv", show_col_types = F)
  horiz = 35
  step = 1
  
  if(unique(forecast$family) == "ensemble"){
    p1 <- forecast %>%
      mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
      ggplot(aes(x = datetime, y = prediction)) +
      geom_hline(yintercept = 0, color = "grey50") +
      geom_vline(xintercept = forecast_date) +
      geom_line(aes(group = parameter), alpha = 0.3) +
      geom_point(data = target %>%
                   mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
                   dplyr::filter(datetime >= as.Date(forecast_date) - hist_data * step,
                                 datetime <= as.Date(forecast_date) + horiz * step), 
                 aes(x = datetime, y = observation, alpha = datetime >= forecast_date)) +
      scale_alpha_manual(values = c(1, .5)) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      facet_wrap(~site_id) +
      ggtitle(paste0("Forecasts for ", forecast_date),
              paste0("Model: ", model_id))
  } else {
    p1 <- forecast %>%
      mutate(site_id = factor(site_id, levels = chamber_levels,
                              labels = chamber_labels)) %>%
      pivot_wider(names_from = "parameter", values_from = "prediction") %>%
      ggplot(aes(x = datetime)) +
      geom_hline(yintercept = 0, color = "grey50") +
      geom_vline(xintercept = forecast_date) +
      geom_line(aes(y = mu)) +
      geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma), alpha = 0.3) +
      geom_point(data = target %>%
                   mutate(site_id = factor(site_id, levels = chamber_levels,
                                           labels = chamber_labels)) %>%
                   dplyr::filter(datetime >= as.Date(forecast_date) - hist_data * step,
                                 datetime <= as.Date(forecast_date) + horiz * step), 
                 aes(x = datetime, y = observation, alpha = datetime >= forecast_date)) +
      scale_alpha_manual(values = c(1, .2)) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      ylab("Flux (µmol/m2/day)") +
      facet_wrap(~site_id) +
      ggtitle(paste0("Forecasts for ", forecast_date),
              paste0("Model: ", model_id))
  }
  
  return(p1)
}
