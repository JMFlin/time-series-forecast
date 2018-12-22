setwd("C:/Users/janne/Documents/time-series-forecast")

# TODO: remove high correlation by pca, tsne and autoencoders or https://shiring.github.io/forecasting/2017/06/09/retail_forcasting_part2
# TODO: library(tsfeatures)
# TODO: tsfresh

# Load libraries
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
  "thief", # Forecasting models and predictions package
  "caret", # Awesome ML Library
  "prophet", # GAM time series modeling
  "doParallel", # Allow parallel processing with caret
  "recipes" # Carry transformation to new data
)

usethis::use_tidy_style()
# reprex::reprex(style = TRUE)

file.sources <- list.files("utils",
  pattern = "*.R$", full.names = TRUE,
  ignore.case = TRUE
)

invisible(sapply(file.sources, source, .GlobalEnv))

h2o.init() # Fire up h2o
h2o.no_progress() # Turn off output of progress bars

# How many steps to forecast at the end
n <- 3

# Specify unit of measurement
unit.of.measurement <- "unit"

# Minimum number of unique instances turned into factors
factor.limit <- 3

# Control variable for caret
regression.control <- trainControl(
  method = "cv",
  number = 3
)

# Allow multiprocessing for caret
cl <- makeCluster(detectCores())
registerDoParallel(cl)

Main <- function() {
  flog.info("Loading data")
  forecast.data <- LoadData(unit.of.measurement)

  data.frequency <- forecast.data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    select(scale)

  flog.info("Plotting time series")
  InitialPlot(forecast.data, data.frequency)

  flog.info("Cleaning target data")
  forecast.data.cleaned <- CleanTarget(forecast.data, data.frequency)

  if (data.frequency == "hour") {
    seasonal.periods <<- c(24, 168)
  } else if (data.frequency == "day") {
    seasonal.periods <<- c(7, 365.25)
  } else if (data.frequency == "week") {
    seasonal.periods <<- c(52, 12)
  } else if (data.frequency == "month") {
    seasonal.periods <<- c(12, 1)
  }

  max.lag <<- round(nrow(forecast.data.cleaned) * 0.3)

  optimal.lag.setting <<- forecast.data.cleaned %>%
    TidyAcf(unit, lags = 1:max.lag) %>%
    filter(acf == max(acf)) %>%
    pull(lag)

  flog.info("Plotting ACF")
  AcfPlot(forecast.data.cleaned)

  flog.info("Creating timetk features")
  forecast.data.features <- CreateTimeTkFeatures(forecast.data.cleaned)

  flog.info("Cleaning features data")
  forecast.data.cleaned <- CleanFeatures(forecast.data.features)

  flog.info("Starting multivariate modeling with h2o")
  MultivariateSeriesH2O(forecast.data.cleaned)

  flog.info("Starting multivariate modeling with lm")
  MultivariateSeriesLM(forecast.data.cleaned)

  flog.info("Starting univariate modeling with arima, ets and tbats")
  UnivariateSeries(forecast.data.cleaned)

  flog.info("Starting univariate modeling with prophet")
  UnivariateProphet(forecast.data.cleaned)
}

Main()

stopCluster(cl)
rm(list = ls())
.rs.restartR()
