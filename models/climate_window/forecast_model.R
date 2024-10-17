# climate window
# written by ASL


#### Step 0: load packages

library(tidyverse)
library(forecast)

#### Step 1: Set model specifications
model_id <- "climate_window"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
window = 10 #Number of days to use in the climate window

#### Step 2: Define the forecast model
forecast_model <- function(site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_daily = NULL,
                           target,
                           horiz,
                           step,
                           forecast_date) {
  
  message(paste0("Running site: ", site))
  
  # Format site data for arima model
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Format
  site_target_raw <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) == 0){
    message(paste0("No target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }

  site_target = site_target_raw |>
    complete(datetime = full_seq(datetime, 1), site_id)

  h = as.numeric(forecast_date - max(site_target$datetime)+horiz)
  
  days <- data.frame(doy = seq(1,366, 1), site_id = site)

  # calculate the mean and standard deviation for each doy
  target_clim <- site_target %>%
    mutate(doy = yday(datetime)) %>%
    #Wrap around doys for rolling mean
    mutate(dup = ifelse(doy <= window/2, 2, 1)) %>%
    uncount(dup, .id = "dup") %>%
    mutate(doy = ifelse(dup>1, doy + 365, doy)) %>%
    mutate(dup = ifelse(doy >= 365- window/2, 2, 1)) %>%
    uncount(dup, .id = "dup") %>%
    mutate(doy = ifelse(dup>1, doy - 365, doy)) %>%
    arrange(doy) %>%
    #Calculate rolling mean
    mutate(clim_mean = slider::slide_index_dbl(.x = get(!!var), 
                                               .i = doy, 
                                               .f = mean, 
                                               na.rm = T,
                                               .before = window/2,
                                               .after = window/2),
           clim_sd = slider::slide_index_dbl(.x = get(!!var), 
                                             .i = doy, 
                                             .f = sd, 
                                             na.rm = T,
                                             .before = window/2,
                                             .after = window/2)) %>%
    filter(doy >= 1 & doy <= 366) %>%
    select(-any_of(c("dup", var))) %>%
    full_join(days, by = c("doy", "site_id")) %>%
    select(-datetime) %>%
    distinct()
  
  # what dates do we want a forecast of?
  forecast_dates <- (1:h)*step+max(site_target$datetime)
  forecast_doy <- yday(forecast_dates)
  
  # put in a table
  forecast_dates_df <- tibble(datetime = forecast_dates,
                              doy = forecast_doy)
  
  forecast <- target_clim %>%
    mutate(doy = as.integer(doy)) %>%
    filter(doy %in% forecast_doy) %>%
    full_join(forecast_dates_df, by = c('doy')) %>%
    arrange(site_id, datetime)
  
  if(sum(!is.na(forecast$clim_mean)) == 0 | sum(!is.na(forecast$clim_sd)) == 0){
    message(paste0("Insufficient historical observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }
  
  # Interpolate
  combined <- forecast %>%
    select(datetime, site_id, clim_mean, clim_sd) %>%
    rename(mean = clim_mean,
           sd = clim_sd) %>%
    mutate(mu = imputeTS::na_interpolation(x = mean),
           sigma = median(sd, na.rm = TRUE))
  
  forecast = data.frame(project_id = "gcrew",
                        model_id = model_id,
                        datetime = (1:h)*step+max(site_target$datetime),
                        reference_datetime = forecast_date,
                        duration = "P1D",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(combined$mu),
                        sigma = as.numeric(combined$sigma)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

