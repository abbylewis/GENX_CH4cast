#' Convert units and names to match VERA targets and calculate daily output
#'
#' Output units:
#' * air_temperature: C
#' * relative_humidity: %
#' * precipitation_flux: mm d-1
#' * wind_speed: m s-1
#'
#' Output is daily mean for all variables except precipitation, which is daily sum
#'
#' @param df EFI standard df
#'
#' @return data frame
#' @export
convert_to_vera_met_P1D <- function(df){
  weather_dat <- df |>
    # rename variables to match met station
    mutate(variable = ifelse(variable == "temperature_2m",
                             "AirTemp_C_mean", variable),
           variable = ifelse(variable == "precipitation",
                             "Rain_mm_sum", variable),
           variable = ifelse(variable == "relativehumidity_2m",
                             "RH_percent_mean", variable),
           variable = ifelse(variable == "windspeed_10m",
                             "WindSpeed_ms_mean", variable),
           variable = ifelse(variable == "shortwave_radiation",
                             "ShortwaveRadiation_Wm2", variable)) %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by_at(colnames(df)[colnames(df) != "prediction"]) %>%
    summarise(sum_pred = sum(prediction),
              prediction = mean(prediction, na.rm = T),
              .groups = "drop") %>%
    mutate(prediction = ifelse(variable == "Rain_mm_sum", sum_pred, prediction)) %>%
    select(-sum_pred)
}
