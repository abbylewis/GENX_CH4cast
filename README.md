# GCReW CH4 forecasting

Forecasts of CH4 emission from GENX experiment at GCReW.

## Models currently set up:

-   auto.arima
-   ets
-   tbats
-   temp.lm
-   met.lm
-   met.lm.step

## TO DO:

1.  Figure out other models to add

## To add new models

1.  Copy an existing model folder (I suggest `ARIMA` for a time series model or `temp_lm` for a model with meteorology)
2.  Update `forecast_model.R` to include your new model (most of the script should be able to stay the same)
3.  Update the the file paths in `rerun_forecasts.R` and `run_forecast.R` to source the correct `forecast_model.R` script
4.  Use `test_forecast.R` to plot forecasts using this model and make sure everything looks right
5.  If that worked, you should be pretty much set up! To create automation, go to `.github/workflows` and copy one of the `do_prediction_XXX.yml` files. Update the model name and file path for your new model.
6.  Push changes and create automation
