get_ensemble_forecast <- function (forecast_date, latitude, longitude, site_id = NULL, forecast_days, 
                                   past_days, model = "gfs_seamless", variables = c("relative_humidity_2m", 
                                                                                    "precipitation", "wind_speed_10m", "cloud_cover", "temperature_2m", 
                                                                                    "shortwave_radiation")) {
  if(forecast_date < Sys.Date()){
    stop("I know this script is set up to accept this, but open meteo is not actually producing a historical forecast!! This is just ensemble met")
  }
  forecast_date <- as.Date(forecast_date)
  if (forecast_days > 35) 
    stop("forecast_days is longer than avialable (max = 35")
  if (past_days > 92) 
    stop("hist_days is longer than avialable (max = 92)")
  if (Sys.Date() - (forecast_date - past_days) > 92) {
    message("forecast date is in the past so less than 92 days of historical data will be returned")
    past_days <- as.numeric(92 - (Sys.Date() - forecast_date)) 
  }
  latitude <- round(latitude, 2)
  longitude <- round(longitude, 2)
  start_date <- forecast_date - past_days
  end_date <- forecast_date + forecast_days
  if (longitude > 180) 
    longitude <- longitude - 360
  if ("shortwave_radiation" %in% variables & model == "ecmwf_ifs04") {
    message("shortwave radiation is not aviailable for ecmwf_ifs04 model")
  }
  variables_api <- paste(variables, collapse = ",")
  url_base <- "https://ensemble-api.open-meteo.com/v1/ensemble"
  url_path <- glue::glue("?latitude={latitude}&longitude={longitude}&hourly={variables_api}&windspeed_unit=ms&forecast_days={forecast_days}&past_days={past_days}&models={model}")
  v <- jsonlite::fromJSON(paste0(url_base, url_path))
  units <- dplyr::tibble(variable = stringr::str_split_i(names(v$hourly), "_member", 1), 
                         unit = unlist(v$hourly_units)) %>%
    distinct() %>%
    filter(variable != "time")
  
  df <- as_tibble(v$hourly) %>%
    mutate(time = lubridate::as_datetime(paste0(time, ":00"))) %>%
    pivot_ensemble_forecast() %>%
    rename(datetime = time) %>%
    mutate(model_id = model, 
           reference_datetime = forecast_date) %>%
    left_join(units, by = "variable") %>%
    mutate(site_id = ifelse(is.null(site_id), paste0(latitude, "_", longitude), site_id)) %>%
    select(c("datetime", "reference_datetime", "site_id", "model_id", "ensemble", 
             "variable", "prediction", "unit"))
  return(df)
}

pivot_ensemble_forecast <- function(df){
  df |>
    tidyr::pivot_longer(-time, names_to = "variable_ens", values_to = "prediction") |>
    dplyr::mutate(
      variable = stringr::str_split(
        variable_ens,
        pattern = "_member",
        n = 2,
        simplify = TRUE
      )[, 1],
      ensemble = stringr::str_split(
        variable_ens,
        pattern = "_member",
        n = 2,
        simplify = TRUE
      )[, 2],
      ensemble = ifelse(ensemble == "", "00",ensemble)) |>
    dplyr::select(-variable_ens)
}
