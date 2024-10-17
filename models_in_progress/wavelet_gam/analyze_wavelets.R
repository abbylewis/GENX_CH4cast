library(waveslim)

analyze_wavelets <- function(target, site, var_name) {
  #Format data (dealing with irregularly spaced data)
  test_data <- target %>%
    filter(site_id == site) %>%
    filter(!is.na(!!sym(var_name)))
  
  #Run wavelet transformation
  data <- zoo::na.approx(test_data[[var_name]])
  wavelet <- waveslim::modwt(data, "la8")
  
  J <- length(wavelet)
  meshed <- as.data.frame(wavelet[1:J]) %>%
    mutate(datetime = test_data$datetime,
           data = data) %>%
    pivot_longer(cols = -c(data, datetime))
  
  df <- meshed %>% 
    mutate(site_id = site)
  
  return(df)
}
