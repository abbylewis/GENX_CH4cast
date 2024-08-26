# VERA meteor strike

Simple empirical forecasts for the VERA forecasting challenge (<https://www.ltreb-reservoirs.org/>)

## Models currently set up:

-   asl.auto.arima
-   asl.ets
-   asl.tbats
-   asl.temp.lm
-   asl.met.lm
-   asl.met.lm.step

## TO DO:

1.  Figure out if we want to add auto_adam, nnetar
2.  Load inflow targets

## To add new models

1.  Copy an existing model folder (I suggest `ARIMA` for a time series model or `temp_lm` for a model with meteorology)
2.  Update `forecast_model.R` to include your new model (most of the script should be able to stay the same)
3.  Update the the file paths in `rerun_forecasts.R` and `run_forecast.R` to source the correct `forecast_model.R` script
4.  Run everything in your `forecast_model.R` file, then open `.R/generate_tg_forecast` and run the lines of this function individually to make sure everything works.
5.  If that worked, you should be pretty much set up! To create automation, go to `.github/workflows` and copy one of the `do_prediction_XXX.yml` files. Update the model name and file path for your new model.
6.  Push changes and create automation
