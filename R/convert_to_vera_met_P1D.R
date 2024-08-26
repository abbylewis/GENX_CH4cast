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
    mutate(variable = ifelse(variable == "air_temperature",
                             "AirTemp_C_mean", variable),
           prediction = ifelse(variable == "AirTemp_C_mean",
                               prediction - 273.15, prediction), #Update units from K to C
           variable = ifelse(variable == "precipitation_flux",
                             "Rain_mm_sum", variable),
           prediction = ifelse(variable == "Rain_mm_sum",
                               prediction * 60 * 60, prediction), #Update units from kg/m2/s to mm/d
           variable = ifelse(variable == "relative_humidity",
                             "RH_percent_mean", variable),
           prediction = ifelse(variable == "RH_percent_mean",
                               prediction * 100, prediction), #Update units from proportion to %
           variable = ifelse(variable == "wind_speed",
                             "WindSpeed_ms_mean", variable)) %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by_at(colnames(df)[colnames(df) != "prediction"]) %>%
    summarise(sum_pred = sum(prediction),
              prediction = mean(prediction, na.rm = T),
              .groups = "drop") %>%
    mutate(prediction = ifelse(variable == "Rain_mm_sum", sum_pred, prediction)) %>%
    select(-sum_pred)
}
