fit_tides <- function(tide_noaa){
  #Prep noaa tides
  tide_formatted <- tide_noaa %>%
    rename(time2 = `Date Time`) %>%
    group_by(time2) %>%
    summarize(corrected_depth = mean(Prediction, na.rm = TRUE)) %>%
    select(time2, corrected_depth) %>%
    rename(datetimestamp = time2, depth = corrected_depth) %>%
    mutate(datetimestamp = as.POSIXct(datetimestamp, tz = "GMT")) %>%
    swmpr(meta_in = "gcrew") %>%
    setstep(timestep = 60) %>% 
    na.approx(maxgap = 1e6)
  
  # get model
  datsl <- as.sealevel(elevation = tide_formatted$depth, time = tide_formatted$datetimestamp)
  mod <- tidem(t = datsl)
  return(mod)
}
