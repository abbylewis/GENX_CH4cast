fit_discharge <- function(depth_without_tides, open_meteo_discharge) {
  #Daily timestep
  all <- depth_without_tides %>%
    mutate(Date = as.Date(time2)) %>%
    group_by(Date) %>%
    summarize(tide_removed = mean(tide_removed, na.rm = TRUE),
              scaled_tide = mean(scaled_tide, na.rm = T)) %>%
    left_join(open_meteo_discharge, by = c("Date" = "date"))
  
  #Make sure we have regular timesteps
  all_reg <- data.frame(Date = seq(all$Date[1], 
                                   all$Date[nrow(all)], 
                                   by = "1 day")) %>%
    left_join(all)
  
  # What lag is best
  n = 10
  vals <- data.frame(lag = 0:n, cor = NA)
  for(i in 0:n) {
    cor <- cor(x = lag(all_reg$tide_removed, i), y = all_reg$river_discharge, use = "complete.obs", method = "spearman")
    vals$cor[i] <- cor
  }
  message(paste0("Using optimal lag: ", vals$lag[which.max(vals$cor)], " days"))
  
  river_mod <- lm(tide_removed ~ log(river_discharge), data = all)
  
  river_preds <- all %>%
    mutate(tide_removed_pred = predict(river_mod, newdata = all),
           final_pred = scaled_tide + tide_removed_pred)
  
  return(river_preds)
}
