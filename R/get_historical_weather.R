get_historical_weather <- function (latitude, longitude, site_id = NULL, start_date, end_date, 
                                    variables = c("relative_humidity_2m", "precipitation", "wind_speed_10m", 
                                                  "cloud_cover", "temperature_2m", "shortwave_radiation")) 
{
  if (start_date < "1950-01-01") 
    warning("start date must be on or after 1950-01-01")
  latitude <- round(latitude, 2)
  longitude <- round(longitude, 2)
  if (longitude > 180) 
    longitude <- longitude - 360
  df <- NULL
  units <- NULL
  url_base <- "https://archive-api.open-meteo.com/v1/archive"
  for (variable in variables) {
    url_path <- glue::glue("?latitude={latitude}&longitude={longitude}&start_date={start_date}&end_date={end_date}&hourly={variable}&windspeed_unit=ms")
    v <- read_url(url_base, url_path)
    units <- dplyr::bind_rows(units, dplyr::tibble(variable = names(v$hourly)[2], 
                                                   unit = unlist(v$hourly_units[2][1])))
    v1 <- dplyr::mutate(dplyr::as_tibble(v$hourly), time = lubridate::as_datetime(paste0(time, 
                                                                                         ":00")))
    if (variable != variables[1]) {
      v1 <- dplyr::select(v1, -time)
    }
    df <- dplyr::bind_cols(df, v1)
  }
  df <- dplyr::select(dplyr::mutate(dplyr::left_join(dplyr::mutate(dplyr::rename(tidyr::pivot_longer(df, 
                                                                                                     -time, names_to = "variable", values_to = "prediction"), 
                                                                                 datetime = time), model_id = "ERA5"), units, by = "variable"), 
                                    site_id = ifelse(is.null(site_id), paste0(latitude, "_", 
                                                                              longitude), site_id)), c("datetime", "site_id", "model_id", 
                                                                                                       "variable", "prediction", "unit"))
  return(df)
}