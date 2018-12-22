setwd("C:/Users/janne/Documents/time-series-forecast")

# TODO: remove high correlation by pca, tsne and autoencoders or https://shiring.github.io/forecasting/2017/06/09/retail_forcasting_part2
# TODO: library(tsfeatures)
# TODO: prophet
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

n <- 3

unit.of.measurement <- "unit"

factor.limit <- 3

regression.control <- trainControl(
  method = "cv",
  number = 3
)


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

  max.lag <<- round(nrow(forecast.data.cleaned) * 0.4)

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
}



Main()

stopCluster(cl)
rm(list = ls())
.rs.restartR()
