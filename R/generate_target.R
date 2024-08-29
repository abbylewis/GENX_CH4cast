source("R/qaqc.R")

generate_target <- function(save = F){
  raw <- read_csv("Raw_data/GENX_Flux_SD_Loggernet_2021-05-05_to_2022-12-31_derived.csv", show_col_types = FALSE) 
  raw2 <- read_csv("Raw_data/GENX_Flux_SD_Loggernet_2023-01-01_to_2023-11-15_derived.csv", show_col_types = FALSE)
  
  comb <- bind_rows(raw, raw2)
  
  target <- comb %>%
    mutate(time2 = with_tz(time2, tzone = "America/New_York"),
           project_id = "gcrew",
           duration = "P1D",
           time2 = as.Date(time2)) %>%
    qaqc() %>%
    rename(site_id = chamber_treatment,
           datetime = time2) %>%
    select(project_id, site_id, datetime, duration, CH4_slope_umol_per_day) %>%
    pivot_longer(cols = CH4_slope_umol_per_day, 
                 names_to = "variable", values_to = "observation") %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by(project_id, site_id, datetime, duration, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")
  
  if(save){
    write.csv(target, "L1.csv", row.names = FALSE)
  }
  return(target)
}
