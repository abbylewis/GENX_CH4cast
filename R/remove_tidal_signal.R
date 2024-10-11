remove_tidal_signal <- function(drivers, mod, end_date){
  #Format end date
  end_date <- as.POSIXct(paste0(end_date, " 23:45:00"), 
                         format = "%Y-%m-%d %H:%M:%S")
  
  #Make tide predictions that match with our depth measurements
  with_tides <- drivers %>%
    group_by(time2) %>%
    summarize(corrected_depth = mean(corrected_depth, na.rm = TRUE)) %>% #Summarize across all chambers
    select(time2, corrected_depth) %>%
    complete(time2 = seq(min(time2), end_date, by = "15 min")) %>%
    mutate(tide_pred = predict(mod, newdata = time2)) %>% #depth_pred is predicted tide
    arrange(time2)
  
  # What lag is best
  n = 200
  vals <- data.frame(lag = 1:n, cor = NA)
  for(i in 1:n) {
    cor <- cor(x = lag(with_tides$corrected_depth, i), y = with_tides$tide_pred, use = "complete.obs", method = "spearman")
    vals$cor[i] <- cor
  }
  message(paste0("Using optimal time lag of ", vals$lag[which.max(vals$cor)], " timesteps"))
  
  #Create a model to remove the tidal signal from the data
  correct_tide_mod <- lm(lag(corrected_depth, 14) ~ tide_pred, data = with_tides)
  
  output <- with_tides %>%
    mutate(scaled_tide = predict(correct_tide_mod, newdata = with_tides),
           tide_removed = corrected_depth - scaled_tide)
  return(output)
}
