
library(tidyverse)
library(sf)
library(ncdf4)

#https://noaa-gestofs-pds.s3.amazonaws.com/README.html
load_and_save_etss <- function(dates){
  for(date in dates){
    date <- as.Date(date)
    url <- paste0("https://noaa-gestofs-pds.s3.amazonaws.com/stofs_2d_glo.",  
                  format(date, "%Y%m%d"),
                  "/stofs_2d_glo.t00z.points.cwl.nc")
    destfile <- tempfile()
    download.file(url, destfile, mode = "wb")
    nc <- nc_open(destfile)
    station_id <- which(grepl("Annapolis", ncvar_get(nc, "station_name")))
    zetas <- ncvar_get(nc,"zeta", start = c(station_id, 1), count = c(1,-1))
    times <- as_datetime("2024-04-04 12:00:00")+ seconds(ncvar_get(nc,"time"))
    data <- data.frame(datetime = times,
                       prediction = zetas)
    formatted <- data %>%
      mutate(model_id = "noaa_gestofs",
             variable = "cwl") |>
      mutate(datetime = as.Date(datetime, tz = "America/New_York"),
             horizon = as.numeric(difftime(datetime,
                                           date, 
                                           units = "days"))) %>%
      group_by_at(colnames(.)[colnames(.) != "prediction"]) %>%
      summarise(prediction = mean(prediction, na.rm = T),
                .groups = "drop") %>%
      filter(horizon >= 0)
    
    write_csv(formatted, 
              here::here("wl_forecasts", 
                         paste0("etss_future_daily_", format(date, "%Y%m%d"), ".csv")))
  }
}

target <- read_csv(here::here("L1_target.csv"), show_col_types = F)
dates <- seq(min(target$datetime), Sys.Date() - 1L, by = "1 day")
processed <- as.Date(str_extract(list.files(here::here("wl_forecasts")), "[0-9]+"),
                     format = "%Y%m%d")
dates <- dates[!dates %in% processed]
#Process all
comb <- load_and_save_etss(dates)
