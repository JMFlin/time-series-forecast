MultivariateSeriesH2O <- function(forecast.data.cleaned) {
  flog.info("Plotting training strategy for h2o")
  TrainingStrategy(forecast.data.cleaned)

  flog.info("Starting predictions for h2o")
  H2O.model <- ModelH2O(forecast.data.cleaned)

  flog.info("Investigating test error")
  error.tbl.h2o <- Evaluate(forecast.data.cleaned, H2O.model$predictions.tbl.h2o)
  print(error.tbl.h2o)

  flog.info("Plotting acutal vs predicted")
  ActualVsPredicted(forecast.data.cleaned, H2O.model$predictions.tbl.h2o)

  flog.info("Ceating future data for prediction")
  feature.data.tbl <- CreateFutureData(forecast.data.cleaned) %>%
    predict(preProc, .)

  h2o.feature.data.tbl <- feature.data.tbl %>%
    select_if(~ !is.Date(.))

  flog.info(glue("Predicting {1:max(n)} steps with h2o: {feature.data.tbl$date}"))
  pred.h2o <- predict(H2O.model$automl.leader, newdata = as.h2o(h2o.feature.data.tbl)) %>%
    as.vector()

  final.tbl <- feature.data.tbl %>%
    mutate(pred = pred.h2o) %>%
    dplyr::select(date, pred)

  flog.info("Plotting true forecasts for h2o")
  TrueForecasts(forecast.data.cleaned, H2O.model$predictions.tbl.h2o, final.tbl)
}


MultivariateSeriesLM <- function(forecast.data.cleaned) {
  flog.info("Starting predictions for lm")
  LM.model <- ModelLM(forecast.data.cleaned)

  flog.info("Investigating test error")
  error.tbl.lm <- Evaluate(forecast.data.cleaned, LM.model$predictions.tbl.lm)
  print(error.tbl.lm)

  flog.info("Plotting actual vs predicted")
  ActualVsPredicted(forecast.data.cleaned, LM.model$predictions.tbl.lm)

  flog.info("Ceating future data for prediction")
  feature.data.tbl <- CreateFutureData(forecast.data.cleaned) %>%
    predict(preProc, .)

  flog.info(glue("Predicting {1:max(n)} steps with lm: {feature.data.tbl$date}"))
  pred.lm <- predict(LM.model$fit.lm, newdata = feature.data.tbl %>%
    select_if(~ !is.Date(.)))

  final.tbl <- feature.data.tbl %>%
    mutate(pred = pred.lm) %>%
    dplyr::select(date, pred)

  flog.info("Plotting true forecasts for lm")
  TrueForecasts(forecast.data.cleaned, LM.model$predictions.tbl.lm, final.tbl)
}


UnivariateSeries <- function(forecast.data.cleaned) {
  flog.info("Starting univariate modeling")
  TS.model <- ModelUnivariate(forecast.data.cleaned)

  flog.info("Investigating test error")
  error.tbl.lm <- Evaluate(forecast.data.cleaned, TS.model$predictions.tbl.uni)
  print(error.tbl.lm)

  flog.info("Plotting actual vs predicted")
  ActualVsPredicted(forecast.data.cleaned, TS.model$predictions.tbl.uni)

  new.data.tbl <- forecast.data.cleaned %>%
    tk_index() %>%
    tk_make_future_timeseries(n_future = max(n))

  flog.info(glue("Predicting {max(n)} steps with {TS.model$fit.uni %>%
    mutate(fcast = map(fit, forecast, h = max(n) + 1)) %>%
                 mutate(sweep = map(fcast, sw_sweep)) %>%
                 unnest(sweep) %>%
                 slice(1) %>%
                 select(model_names) %>%
                 as_data_frame() %>% as.character}"))

  pred.uni <- TS.model$fit.uni %>%
    mutate(fcast = map(fit, forecast, h = max(n) + 1)) %>%
    mutate(sweep = map(fcast, sw_sweep, fitted = FALSE, timetk_idx = TRUE, rename_index = "date")) %>%
    unnest(sweep) %>%
    slice((n() - (max(n) - 1)):n()) %>%
    dplyr::select(value) %>%
    as_vector()

  final.tbl <- data.frame(
    date = new.data.tbl,
    pred = pred.uni
  ) %>%
    as.tibble()

  flog.info("Plotting true forecasts for univariate model")
  TrueForecasts(forecast.data.cleaned, TS.model$predictions.tbl.uni, final.tbl)
}

UnivariateProphet <- function(forecast.data.cleaned) {
  flog.info("Starting prophet modeling")
  PROPHET.model <- ModelProphet(forecast.data.cleaned)

  flog.info("Investigating test error")
  error.tbl.lm <- Evaluate(forecast.data.cleaned, PROPHET.model$predictions.tbl.uni)
  print(error.tbl.lm)

  flog.info("Plotting actual vs predicted")
  ActualVsPredicted(forecast.data.cleaned, PROPHET.model$predictions.tbl.uni)

  new.data.tbl <- forecast.data.cleaned %>%
    tk_index() %>%
    tk_make_future_timeseries(n_future = max(n)) %>%
    as_data_frame() %>%
    rename("ds" = value)

  flog.info(glue("Predicting {1:max(n)} steps with prophet: {new.data.tbl$ds}"))
  pred.prophet <- predict(PROPHET.model$fit.prophet, new.data.tbl)

  final.tbl <- pred.prophet %>%
    rename(pred = yhat, date = ds) %>%
    dplyr::select(date, pred)

  flog.info("Plotting true forecasts for prophet")
  TrueForecasts(forecast.data.cleaned, PROPHET.model$predictions.tbl.uni, final.tbl)
}
