
generate_target <- function(){
  raw <- read_csv("Raw_data/GENX_Flux_SD_Loggernet_2021-05-05_to_2022-12-31_derived.csv", show_col_types = FALSE) 
  
  target <- raw %>%
    mutate(time2 = with_tz(time2, tzone = "America/New_York"),
           project_id = "gcrew",
           duration = "P1D",
           time2 = as.Date(time2)) %>%
    rename(site_id = zone_name,
           datetime = time2) %>%
    select(project_id, site_id, datetime, duration, CH4_slope_ppm_per_day) %>%
    pivot_longer(cols = CH4_slope_ppm_per_day, 
                 names_to = "variable", values_to = "observation") %>%
    group_by(project_id, site_id, datetime, duration, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")
  
  #write.csv(target, "L1.csv", row.names = FALSE)
  return(target)
}