
qaqc <- function(L0_file = "L0.csv"){
  #Remove wacky starting values
  slopes <- read_csv(L0_file, show_col_types = F) %>%
    dplyr::filter(TIMESTAMP <= as_datetime("2023-03-02") |
                    TIMESTAMP >= as_datetime("2023-06-27"))
  
  #Remove the worst 1% of fits
  fit_cutoff <- quantile(slopes$CH4_rmse, 0.99)
  message(paste0("Removing fits with RMSE > ", round(fit_cutoff, 3), 
                 " (1% of fits; n = ", sum(slopes$CH4_rmse > fit_cutoff), ")"))
  slopes_clean <- slopes %>%
    filter(CH4_rmse < fit_cutoff) %>%
    filter(CH4_init > 1.5,
           CH4_init < 2.5)
  
  #Load metadata for chambers
  metadata <- read_csv(here::here("Raw_data","chamber_metadata.csv"), show_col_types = F)
  slopes_metadata <- metadata %>%
    left_join(slopes_clean %>%
                mutate(MIU_VALVE = as.numeric(MIU_VALVE)), 
              by = c("miu_valve" = "MIU_VALVE")) %>%
    rename(time2 = TIMESTAMP) %>%
    filter(year(time2) >= 2021)
  
  #Output
  write.csv(slopes_metadata, here::here("L1.csv"), row.names = FALSE)
  return(slopes_metadata)
}
