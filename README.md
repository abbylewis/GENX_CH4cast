# GCReW CH4 forecasting

Forecasts of CH4 flux from GENX experiment at GCReW.

## To add new models

1.  Copy an existing model folder (I suggest `models/auto.arima` for a time series model or `models/randomForest` for a model with meteorology)
2.  In that folder, update `forecast_model.R` to include your new model (most of the script should stay the same)
3.  `new_model_sandbox.R` provides some sandbox code to test your model as you work on development
4.  Update the the file paths in `rerun_forecasts.R` and `run_forecast.R` to source the correct `forecast_model.R` script
5.  Use `test_forecast.R` to plot forecasts using this model and make sure everything looks right
6.  If that worked, you should be pretty much set up! To create automation, go to `.github/workflows` and add your model to one of the `do_prediction_XXX.yml` files
7.  Push changes and create automation
