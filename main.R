# Load libraries
setwd("C:/Users/janne/Documents/time-series-forecast")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "h2o", # Awesome ML Library
  "timetk", # Toolkit for working with time series in R
  "futile.logger", # Adds logging
  "tidyquant", # Loads tidyverse, financial pkgs, used to get data
  "janitor", # COlumn name handling
  "glue", # A better paste function
  "styler", # Style r code
  "sweep", # Broom-style tidiers for the forecast package
  "forecast" # Forecasting models and predictions package
)
# library(tsfeatures)

usethis::use_tidy_style()
# reprex::reprex(style = TRUE)

file.sources <- list.files("utils",
  pattern = "*.R$", full.names = TRUE,
  ignore.case = TRUE
)

invisible(sapply(file.sources, source, .GlobalEnv))

h2o.init() # Fire up h2o
h2o.no_progress() # Turn off output of progress bars

n <- c(1, 2, 3)

unit.of.measurement <- "unit"


Main <- function() {
  flog.info("Loading data")
  forecast.data <- LoadData(unit.of.measurement)

  data.frequency <- forecast.data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    select(scale)

  max.lag <- round(nrow(forecast.data) * 0.4)

  flog.info("Plotting time series")
  InitialPlot(forecast.data, data.frequency)

  flog.info("Cleaning data")
  forecast.data.cleaned <- forecast.data %>%
    clean_names() %>%
    remove_empty(c("cols")) %>%
    select_if(~ !any(is.na(.)))

  flog.info("Plotting ACF")
  AcfPlot(forecast.data.cleaned, max.lag)

  flog.info("Starting multivariate modeling with h2o")
  MultivariateSeriesH2O(forecast.data.cleaned, max.lag)

  flog.info("Starting multivariate modeling with lm")
  MultivariateSeriesLM(forecast.data.cleaned, max.lag)

  flog.info("Starting univariate modeling with arima, ets and tbats")
  UnivariateSeries(forecast.data.cleaned, max.lag)
}

CreateTimeTkFeatures <- function(forecast.data.cleaned, max.lag) {
  flog.info("Augmenting data")
  forecast.data.augmented <- forecast.data.cleaned %>%
    tk_augment_timeseries_signature() %>%
    select(-diff)

  flog.info("Cleaning augmented data")
  forecast.data.cleaned <- forecast.data.augmented %>%
    clean_names() %>%
    remove_empty(c("cols")) %>%
    select_if(~ !any(is.na(.))) %>%
    mutate_if(is.ordered, ~ as.character(.) %>% as.factor())

  flog.info("Finding optimal lag")
  optimal.lag.setting <- forecast.data.cleaned %>%
    TidyAcf(unit, lags = 1:max.lag) %>%
    filter(acf == max(acf)) %>%
    pull(lag)

  flog.info("Inserting optimal lag into feature data")
  forecast.data.lagged <- forecast.data.cleaned %>%
    mutate(value.lag = lag(unit, n = optimal.lag.setting)) %>%
    filter(!is.na(value.lag))

  return(forecast.data.lagged)
}

CreateFutureData <- function(forecast.data.cleaned, max.lag) {

  # Retrieves the timestamp information
  forecast.idx <- forecast.data.cleaned %>%
    tk_index()

  optimal.lag.setting <- forecast.data.cleaned %>%
    TidyAcf(unit, lags = 1:max.lag) %>%
    filter(acf == max(acf)) %>%
    pull(lag)

  flog.info("Creating future time indexes")
  # Make future index
  new.data.tbl <- forecast.idx %>%
    tk_make_future_timeseries(n_future = 12) %>%
    tk_get_timeseries_signature() %>%
    slice(n) %>%
    mutate(date = index) %>%
    select(-diff, -index) %>%
    mutate_if(is.ordered, ~ as.character(.) %>% as.factor()) %>%
    clean_names()

  flog.info("Creating feature table")
  feature.data.tbl <- inner_join(new.data.tbl, forecast.data.cleaned %>%
    mutate(date = as.Date(floor_date(date + months(optimal.lag.setting, abbreviate = FALSE), unit = "month"))) %>%
    mutate(value.lag = unit) %>%
    select(date, value.lag), by = c("date"))

  return(feature.data.tbl)
}

MultivariateSeriesH2O <- function(forecast.data.cleaned, max.lag) {
  flog.info("Starting to create timetk features")
  forecast.data.lagged <- CreateTimeTkFeatures(forecast.data.cleaned, max.lag)

  flog.info("Plotting training strategy for h2o")
  TrainingStrategy(forecast.data.lagged)

  flog.info("Starting predictions for h2o")
  H2O.model <- ModelH2O(forecast.data.lagged)

  flog.info("Investigating test error")
  error.tbl.h2o <- Evaluate(forecast.data.cleaned, H2O.model$predictions.tbl.h2o)
  print(error.tbl.h2o)

  flog.info("Plotting acutal vs predicted")
  ActualVsPredicted(forecast.data.cleaned, H2O.model$predictions.tbl.h2o)

  flog.info("Ceating future data for prediction")
  feature.data.tbl <- CreateFutureData(forecast.data.cleaned, max.lag)

  h2o.feature.data.tbl <- feature.data.tbl %>%
    select_if(~ !is.Date(.))

  flog.info(glue("Predicting {max(n)} steps ahead with h2o"))
  pred.h2o <- predict(H2O.model$automl.leader, newdata = as.h2o(h2o.feature.data.tbl)) %>%
    as.vector()

  final.tbl <- feature.data.tbl %>%
    mutate(pred = pred.h2o) %>%
    select(date, pred)

  flog.info("Plotting true forecasts for h2o")
  TrueForecasts(forecast.data, H2O.model$predictions.tbl.h2o, final.tbl)
}



MultivariateSeriesLM <- function(forecast.data.cleaned, max.lag) {
  flog.info("Starting to create timetk features")
  forecast.data.lagged <- CreateTimeTkFeatures(forecast.data.cleaned, max.lag)

  flog.info("Starting predictions for lm")
  LM.model <- ModelLM(forecast.data.lagged)

  flog.info("Investigating test error")
  error.tbl.lm <- Evaluate(forecast.data.cleaned, LM.model$predictions.tbl.lm)
  print(error.tbl.lm)

  flog.info("Plotting actual vs predicted")
  ActualVsPredicted(forecast.data.cleaned, LM.model$predictions.tbl.lm)

  flog.info("Ceating future data for prediction")
  feature.data.tbl <- CreateFutureData(forecast.data.cleaned, max.lag)

  flog.info(glue("Predicting {max(n)} steps ahead with lm"))
  pred.lm <- predict(LM.model$fit.lm, newdata = feature.data.tbl %>%
    select_if(~ !is.Date(.)))

  final.tbl <- feature.data.tbl %>%
    mutate(pred = pred.lm) %>%
    select(date, pred)

  flog.info("Plotting true forecasts for lm")
  TrueForecasts(forecast.data, LM.model$predictions.tbl.lm, final.tbl)
}




UnivariateSeries <- function(forecast.data.cleaned, max.lag) {
  flog.info("Finding frequency")
  data.frequency <- forecast.data.cleaned %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    select(scale) %>%
    mutate(scale = ifelse(scale == "day", 365,
      ifelse(scale == "week", 7,
        ifelse(scale == "month", 12,
          ifelse(scale == "year", 1, NA)
        )
      )
    )) %>%
    as_vector()

  flog.info("Starting univariate modeling")
  TS.model <- ModelUnivariate(forecast.data.cleaned, data.frequency)

  flog.info("Investigating test error")
  error.tbl.lm <- Evaluate(forecast.data.cleaned, TS.model$predictions.tbl.uni)
  print(error.tbl.lm)

  flog.info("Plotting actual vs predicted")
  ActualVsPredicted(forecast.data.cleaned, TS.model$predictions.tbl.uni)

  new.data.tbl <- forecast.data.cleaned %>%
    tk_index() %>%
    tk_make_future_timeseries(n_future = max(n))

  flog.info(glue("Predicting {max(n)} steps ahead with {TS.model$fit.uni %>%
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
    select(value) %>%
    as_vector()

  final.tbl <- data.frame(
    date = new.data.tbl,
    pred = pred.uni
  ) %>%
    as.tibble()

  flog.info("Plotting true forecasts for univariate model")
  TrueForecasts(forecast.data.cleaned, TS.model$predictions.tbl.uni, final.tbl)
}

Main()
