load_river_discharge <- function(forecast_date = Sys.Date(),
                                 forecast_days = 35) {
  
  lat <- 38.87477
  long <- -76.54977
  
  start_date = as.Date("2021-01-01")
  end_date = as.Date(forecast_date + forecast_days)
  
  url_base <- "https://flood-api.open-meteo.com/v1/flood"
  url_path <- glue::glue("?latitude={lat}&longitude={long}&daily=river_discharge&start_date={start_date}&end_date={end_date}")
  v <- read_url(url_base, url_path)
  units <- dplyr::tibble(variable = names(v$daily), 
                         unit = unlist(v$daily_units)) %>%
    distinct() %>%
    filter(!variable == "time")
  
  df <- dplyr::as_tibble(v$daily) %>%
    dplyr::mutate(time = lubridate::as_date(time)) %>%
    rename(date = time) %>%
    mutate(site_id = paste0(lat, "_", long))
  
  return(df)
}

read_url <- function(url_base, url_path){
  out <- tryCatch({
    out <- httr2::request(url_base) |>
      httr2::req_url_path_append(url_path) |>
      httr2::req_throttle(10 / 60, realm = url_base) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)
  }, error = function(err) {
    out <- jsonlite::read_json(file.path(url_base,url_path), simplifyVector = TRUE)
    return(out)
  }, finally = {
    NULL
  })
  
}