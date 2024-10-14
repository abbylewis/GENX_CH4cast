run_PEPRMT <- function(target) {
  #First run GPP Module
  GPP_theta <- c(0.7479271, 1.0497113, 149.4681710, 94.4532674 )
  GPP_mod_target <- PEPRMT_GPP_final(GPP_theta,
                                     data = target)
  
  #Create a new dataset that included model results
  target_results <- target %>%
    left_join(GPP_mod_target %>%
                rename(GPP_mod = GPP,
                       DOY = Time_2),
              by = c("DOY", "site"))
  
  #Second run Reco Module
  #Add modeled GPP into data before running Reco module (16th column)
  target[,16]<-target_results$GPP_mod
  
  Reco_theta <- c(18.41329, 1487.65701, 11.65972, 61.29611 )
  Reco_mod_target <- PEPRMT_Reco_FINAL(Reco_theta,
                                       data = target,
                                       wetland_type=2)
  
  #Create a new dataset that included model results
  target_results<-target_results %>%
    left_join(Reco_mod_target %>%
                rename(DOY = Time_2,
                       Reco_mod = Reco_full),
              by = c("DOY", "site"))
  
  #Last, run CH4 module
  #Add modeled S1, S2 into data before running CH4 module (17th & 18th columns)
  target[,17]<-target_results$S1
  target[,18]<-target_results$S2
  
  CH4_theta<- c( 14.9025078, 0.4644174, 16.7845002, 0.4359649, 15.8857612,
                 0.5120464, 486.4106939, 0.1020278 )
  CH4_mod_target <- PEPRMT_CH4_FINAL(CH4_theta,
                                     data = target,
                                     wetland_type=2)
  
  #Create a new dataset that included model results
  target_results<-target_results %>%
    left_join(CH4_mod_target %>%
                rename(DOY = Time_2,
                       CH4_mod = pulse_emission_total),
              by = c("DOY", "site"))
  
  return(target_results)
}

