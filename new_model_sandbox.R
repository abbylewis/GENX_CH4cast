#### Load packages
source("./R/load_met.R")
source("./R/generate_target.R")
library(tidyverse)
source("./R/generate_target.R")
library(forecast)

### Set model specifications
model_id <- "test"
all_forecast_vars <- read_csv("forecast_variables.csv", show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
boot_number = 100 #Number of bootstraps to run
bootstrap = F
window = 10 #Number of days to use in the climate window

### Download latest target data
target <- generate_target()

### Set forecast specifications
if(sites == "all"){
  sites <- unique(target$site_id)
}
horiz = 35
step = 1
#For testing
site <- "c_10_e4.5"
var <- model_variables[1]
forecast_date <- as.Date("2023-07-01")

### Get NOAA driver data (if needed)
if(noaa){ #Some forecasts do not use any noaa driver data --> in that case skip download
  forecast_date <- as.Date(forecast_date)
  
  #This function loads meteorology and harmonizes past/future predictions
  load_met(forecast_date = forecast_date) 
  
  #Identify available files
  saved_met <- list.files(paste0("./met_downloads/"))
  saved_met_relevant <- saved_met[grepl(forecast_date, saved_met)]
  
  #Load forecasts
  noaa_future_daily <- read_csv(paste0("./met_downloads/",
                                       saved_met_relevant[grepl("future", saved_met_relevant)])) |> 
    mutate(datetime = lubridate::as_date(datetime))
  
  # Load historical data
  noaa_past_mean <- read_csv(paste0("./met_downloads/",
                                    saved_met_relevant[grepl("past", saved_met_relevant)])) |> 
    mutate(datetime = lubridate::as_date(datetime))
  
} else {
  forecast_date <- as.Date(forecast_date)
  noaa_future_daily <- NULL
  noaa_past_mean <- NULL
}

# Format site data
site_target_raw <- target |>
  dplyr::mutate(datetime = as.Date(datetime)) |>
  dplyr::select(datetime, site_id, variable, observation) |>
  dplyr::filter(variable == var, 
                site_id == site,
                datetime < forecast_date) |>
  tidyr::pivot_wider(names_from = "variable", values_from = "observation")

if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) == 0){
  message(paste0("No target observations at site ", site, 
                 ". Skipping forecasts at this site."))
  return()
}

site_target = site_target_raw |>
  complete(datetime = full_seq(datetime, 1), site_id)

h = as.numeric(forecast_date - max(site_target$datetime)+horiz)





### MODIFY



#Wavelet decomposition
dfs <- analyze_wavelets(target = site_target, site = site, var_name = var)

# Fit arima model to each scale
dfs_fit <- dfs %>%
  group_by(site_id, name) %>%
  nest() %>%
  summarize(fit = map(data, ~auto.arima(.x$value)),
            .groups = "drop")

#Function to generate probabilistic forecasts for each scale
pred_wavelet <- function(fit, h, npaths = 100){
  out <- as.data.frame(forecast(fit, h = h, level=0.68)) #One SD
  sim <- matrix(NA, nrow = npaths, ncol = h)
  for (i in 1:npaths){
    sim[i, ] <- simulate(fit, nsim = h, bootstrap = TRUE)
  }
  out <- as.data.frame(t(sim))
  out$datetime <- (1:h)*step+max(site_target$datetime)
  out <- out %>%
    pivot_longer(cols = paste0("V", 1:npaths), names_to = "parameter", values_to = "prediction") %>%
    mutate(parameter = as.numeric(sub("V", "", parameter)))
  return(out)
}

# Forecast
dfs_pred <- dfs_fit %>%
  group_by(name) %>%
  mutate(forecast = map(fit, pred_wavelet, h = h)) %>%
  unnest(forecast) 

#Inverse wavelet transform
run_inverse_wavelet <- function(data){
  data <- ungroup(data) 
  J = length(unique(data$name))
  wave_recon <- list()
  for(jj in 1:J){
    wave_recon[[jj]] <- data %>%
      filter(name == unique(data$name)[jj]) %>%
      pull(prediction)
  }
  names(wave_recon) <- unique(data$name)
  class(wave_recon) <- "modwt"
  attr(wave_recon, "wavelet") <- "la8"
  attr(wave_recon, "boundary") <- "periodic"
  tryCatch(imodwt(wave_recon),
    error = function(cond) {
      message("error")
      return(wave_recon)
    },
    warning = function(cond) {
      message("Warning")
      NULL
    }
  )
  out <- imodwt(wave_recon)
  #return(data.frame(prediction = out,
  #                  datetime = unique(data$datetime)))
  return(NULL)
}

forecast <- dfs_pred %>%
  group_by(parameter) %>%
  nest() %>%
  mutate(prediction = map(data, run_inverse_wavelet)) %>%
  unnest(prediction) %>%
  mutate(project_id = "gcrew",
         model_id = model_id,
         site_id = site,
         reference_datetime = forecast_date,
         duration = "P1D",
         family = "ensemble",
         variable = var)%>%
  select(project_id, model_id, datetime, reference_datetime, duration,
         site_id, family, parameter, variable, prediction)

data <- dfs_pred %>%
  filter(parameter == 89)
