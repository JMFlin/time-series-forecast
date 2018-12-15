# Load libraries
setwd("C:/Users/janne/Documents/time-series-forecast")
library(h2o) # Awesome ML Library
library(timetk) # Toolkit for working with time series in R
library(tidyquant) # Loads tidyverse, financial pkgs, used to get data
library(janitor)
library(glue)
library(futile.logger)
library(tsfeatures)
library(styler)

usethis::use_tidy_style()
reprex::reprex(style = TRUE)

source("R/evaluate.R")
source("R/predict.R")

LoadData <- function(unit) {
  forecast.data <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2017-12-31")

  forecast.data <- forecast.data %>%
    rename(!!unit := price)

  return(forecast.data)
}

TidyAcf <- function(forecast.data, value, lags = 0:20) {
  acf.values <- forecast.data %>%
    pull(unit) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[, , 1]

  ret <- tibble(acf = acf.values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)

  return(ret)
}

InitialPlot <- function(forecast.data, data.frequency) {
  if (data.frequency == "month") {
    breaks <- "1 year"
  } else if (data.frequency == "week") {
    breaks <- "4 month"
  } else if (data.frequency == "day") {
    breaks <- "1 month"
  }

  forecast.data %>%
    ggplot(aes(date, unit)) +
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    theme_tq() +
    # scale_x_date(date_breaks = breaks, date_labels = "%Y") +
    labs(title = "Time series to forecast")
}

Main <- function() {
  h2o.init() # Fire up h2o
  # unit <- "unit"
  n <- c(1, 2, 3)

  flog.info("Loading data")
  forecast.data <- LoadData("unit")

  # SEE SCALE
  data.frequency <- forecast.data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    select(scale)

  max.lag <- round(nrow(forecast.data)*0.4)

  flog.info("Plotting time series")
  InitialPlot(forecast.data, data.frequency)

  flog.info("Augmenting data")
  forecast.data.augmented <- forecast.data %>%
    tk_augment_timeseries_signature() %>%
    select(-diff)

  flog.info("Cleaning data")
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

  flog.info("Plotting ACF")
  forecast.data.cleaned %>%
    TidyAcf(unit, lags = 1:max.lag) %>%
    ggplot(aes(lag, acf)) +
    geom_vline(xintercept = optimal.lag.setting, size = 3, color = palette_light()[[2]]) +
    geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]], size = 2) +
    geom_label(aes(label = acf %>% round(2)),
      vjust = -1, color = palette_light()[[1]]
    ) +
    theme_tq() +
    labs(title = "ACF")

  flog.info("Inserting optimal lag into feature data")
  forecast.data.lagged <- forecast.data.cleaned %>%
    mutate(value.lag = lag(unit, n = optimal.lag.setting)) %>%
    filter(!is.na(value.lag))

  flog.info("Plotting training strategy for h2o")
  TrainingStrategy(forecast.data.lagged)

  flog.info("Starting predictions for h2o")
  H2O.model <- ModelH2O(forecast.data.lagged)

  flog.info("Investigating test error")
  error.tbl.h2o <- Evaluate(forecast.data, H2O.model$predictions.tbl.h2o)
  print(error.tbl.h2o)

  ActualVsPredicted(forecast.data, H2O.model$predictions.tbl.h2o)

  flog.info("Starting predictions for lm")
  LM.model <- ModelLM(forecast.data.lagged)

  flog.info("Investigating test error")
  error.tbl.lm <- Evaluate(forecast.data, LM.model$predictions.tbl.lm)
  print(error.tbl.lm)

  ActualVsPredicted(forecast.data, LM.model$predictions.tbl.lm)

  # Retrieves the timestamp information
  forecast.idx <- forecast.data %>%
    tk_index()

  # Make future index
  new.data.tbl <- forecast.idx %>%
    tk_make_future_timeseries(n_future = 12) %>%
    tk_get_timeseries_signature() %>%
    slice(n) %>%
    mutate(date = index) %>%
    select(-diff, -index) %>%
    clean_names()

  feature.data.tbl <- inner_join(new.data.tbl, forecast.data.cleaned %>%
    mutate(date = as.Date(floor_date(date + months(optimal.lag.setting, abbreviate = FALSE), unit = "month"))) %>%
    mutate(value.lag = unit) %>%
    select(date, value.lag), by = c("date"))

  pred.lm <- predict(LM.model$fit.lm, newdata = feature.data.tbl %>%
    select_if(~ !is.Date(.)))

  final.tbl <- feature.data.tbl %>%
    mutate(pred = pred.lm) %>%
    select(date, pred)

  TrueForecasts(forecast.data, LM.model$predictions.tbl.lm, final.tbl)

  pred.h2o <- predict(H2O.model$automl.leader, newdata = feature.data.tbl %>%
    select_if(~ !is.Date(.)))

  final.tbl <- feature.data.tbl %>%
    mutate(pred = pred.h2o) %>%
    select(date, pred)

  TrueForecasts(forecast.data, H2O.model$automl.leader, final.tbl)
}
