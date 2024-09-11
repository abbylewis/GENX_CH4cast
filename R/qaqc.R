
qaqc <- function(target){
  cleaned <- target %>%
    dplyr::mutate(
      #Generally trust negative fluxes less than daytime fluxes
      keep = ifelse(CH4_slope_umol_per_day < 0 & CH4_rsquared > 0.9, T, F),
      keep = ifelse(CH4_slope_umol_per_day > 0 & CH4_rsquared > 0.75, T, keep),
      #If CO2 is good, we should believe CH4
      keep = ifelse(CH4_slope_umol_per_day > 0 & CO2_rsquared > 0.9, T, keep)) %>%
    dplyr::filter(keep) %>%
    mutate(CH4_slope_umol_per_day = ifelse(time2 > "2023-03-02" & time2 < "2023-06-27",
                                           NA,
                                           CH4_slope_umol_per_day)) %>%
    dplyr::select(-keep)
  
  return(target)
}
