source("R/qaqc.R")
source("R/download_new_data.R")
source("R/calculate_flux.R")

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
generate_target <- function(save = T, 
                            reprocess = F){
  #First - check for new data and download locally
  download <- download_new_data()
  
  #Second- calculate fluxes for new data, re-generating the L0 file
  if(reprocess){
    L0 <- calculate_flux(start_date = "2021-01-01", 
                         end_date = "2024-12-31",
                         modif_start_date = NULL)
  } else {
    L0 <- calculate_flux()
  }
  
  #Third- QAQC
  data <- qaqc("L0.csv") #Running qaqc() is equivalent to reading in L1 file
  
  #Format as target data
  target <- data %>%
    mutate(time2 = with_tz(time2, tzone = "America/New_York"),
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
  
  write.csv(target, "L1_target.csv", row.names = FALSE)
  return(target)
}

target <- generate_target(reprocess = F)
