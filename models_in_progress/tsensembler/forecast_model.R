# tsensembler model
# written by ASL


#### Step 0: load packages

library(tidyverse)
source("./R/generate_target.R")
library(forecast)
library(tsensembler)

#### Step 1: Set model specifications
model_id <- "tsensembler"
all_forecast_vars <- read_csv("forecast_variables.csv", show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data

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
    complete(datetime = full_seq(datetime, 1), site_id) %>%
    filter(!is.na(CH4_slope_umol_per_day))
  
  # NEED TO FIGURE OUT WHAT TO DO ABOUT NAs

  h = as.numeric(forecast_date - max(site_target$datetime)+horiz)

  # Embed time series
  dataset <- embed_timeseries(site_target$CH4_slope_umol_per_day, 5)
  
  # splitting data into train/test
  train <- dataset[1:150,]
  test <- dataset[151:170, ]
  
  # setting up base model parameters
  
  nall_kernels <- c("rbfdot","polydot","vanilladot","laplacedot")
  
  pars_predictors <- list(
    bm_gaussianprocess = list(kernel = nall_kernels, tol = c(.001)),
    bm_svr = list(kernel = nall_kernels, C=c(1, 5), epsilon=c(.1,0.01)),
    bm_ppr = list(nterms = c(2,4),
                  sm.method = c("supsmu","gcvspline")),
    bm_mars = list(degree = c(1, 3), nk = c(10,20),
                   thresh=c(0.001),
                   pmethod=c("forward")),
    bm_glm = list(alpha = c(0,.25,.5,.75,1),
                  family = c("gaussian")),
    bm_randomforest = list(num.trees = c(250,500),
                           mtry = c(5,10)),
    bm_pls_pcr = list(method = c("simpls","kernelpls","svdpc")),
    bm_cubist  = list(committees= c(1,5, 15)),
    bm_xgb = list()
  )
  
  
  base_predictors <- names(pars_predictors)
  
  specs <- model_specs(base_predictors,pars_predictors)
  
  # building the ensemble
  model <- quickADE(target ~., train, specs)
  
  # forecast next value and update base and meta models
  # every three points;
  # in the other points, only the weights are updated
  predictions <- numeric(nrow(test))
  for (i in seq_along(predictions)) {
    predictions[i] <- predict(model, test[i, ])@y_hat
    if (i %% 3 == 0) {
      model <-
        update_base_models(model,
                           rbind.data.frame(train, test[seq_len(i), ]))
      
      model <- update_ade_meta(model, rbind.data.frame(train, test[seq_len(i), ]))
    }
    else
      model <- update_weights(model, test[i, ])
  }
  
  
  # setting up an ensemble of support vector machines
  specs2 <-
    model_specs(learner = c("bm_svr"),
                learner_pars = list(
                  bm_svr = list(kernel = c("vanilladot", "polydot",
                                           "rbfdot"),
                                C = c(1,3,6))
                ))
  
  model <- DETS(target ~., train, specs2)
  preds <- predict(model, test)@y_hat
  
  plot(test$target)
  points(preds, col = "red")
  
  # NEED TO FIGURE OUT UNCERTAINTY
  
  # use the model to forecast target variable
  forecast_raw <- as.data.frame(forecast(fit, h = h, level=0.68)) %>% #One SD
    mutate(sigma = `Hi 68`-`Point Forecast`)
  
  forecast = data.frame(project_id = "gcrew",
                        model_id = model_id,
                        datetime = (1:h)*step+max(site_target$datetime),
                        reference_datetime = forecast_date,
                        duration = "P1D",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(forecast_raw$`Point Forecast`),
                        sigma = as.numeric(forecast_raw$sigma)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

