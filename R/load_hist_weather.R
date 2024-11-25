
library(tidyverse)
source(here::here("R", "megacube_extract.R"))
source(here::here("R", "gefs-methods.R"))

#remember you need to deal with time zones
load_hist_weather <- function(){
  target <- read_csv(here::here("L1_target.csv"), show_col_types = F)
  date <- seq(min(target$datetime), Sys.Date(), by = "1 day")
  
  forecasts <- list.files(here::here("met_downloads"), full.names = T)
  
  past <- forecasts %>%
    map(read_csv, show_col_types = F) %>%
    bind_rows() %>%
    filter(horizon == 0) %>%
    group_by(datetime, variable, site_id, model_id) %>%
    summarize(prediction = mean(prediction, na.rm = T), .groups = "drop")
  
  write_csv(past, here::here("met_downloads", 
                                 paste0("past_daily_current.csv")))
  return(past)
}
