#' generate_target
#'
#' @description
#' This is the master file to load data from dropbox, calculate fluxes, and QAQC the data
#'
#' @param reprocess whether to reprocess files we have already processed
#'
#' @return target data based on all dropbox CH4 flux data
#' @export
#'
#' @examples
generate_target <- function(){
  data <- read_csv("https://raw.githubusercontent.com/abbylewis/GENX_flux_data/refs/heads/master/L1.csv",
                   show_col_types = F)
  
  #Format as target data
  target <- data %>%
    mutate(time2 = with_tz(TIMESTAMP, tzone = "America/New_York"),
           project_id = "gcrew",
           duration = "P1D",
           time2 = as.Date(time2)) %>%
    rename(site_id = chamber_treatment,
           datetime = time2) %>%
    select(project_id, site_id, datetime, duration, CH4_slope_umol_per_day) %>%
    pivot_longer(cols = CH4_slope_umol_per_day, 
                 names_to = "variable", values_to = "observation") %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by(project_id, site_id, datetime, duration, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")
  
  write.csv(target, here::here("L1_target.csv"), row.names = FALSE)
  return(target)
}

#target <- generate_target()
