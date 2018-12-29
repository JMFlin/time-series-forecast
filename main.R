# TODO: remove high correlation by pca, tsne and autoencoders or https://shiring.github.io/forecasting/2017/06/09/retail_forcasting_part2
# TODO: tsfeatures
# TODO: tsfresh
# TODO: preprocessing for all multivariate methods
# TODO: GAM: https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/ and others in blog!!
# TODO: GAM: https://labs.eleks.com/2016/10/combined-different-methods-create-advanced-time-series-prediction.html
# TODO: timeseries cv caret: https://rpubs.com/crossxwill/time-series-cv , https://stackoverflow.com/questions/39571662/model-interpretation-using-timeslice-method-in-caret
# TODO: fourier terms as predictors
# TODO: sin and cos as features: http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html and the other one

# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  
  # Machine learning and forecasting
  "h2o", # Awesome ML Library
  "caret", # Awesome ML Library
  "prophet", # GAM time series modeling
  "forecast", # Forecasting models and predictions package
  
  # Feature creation
  "timetk", # Toolkit for working with time series in R
  "TSA",
  
  # Logging
  "futile.logger", # Adds logging
  
  # Data manipulation
  "tidyquant", # Loads tidyverse, financial pkgs, used to get data
  "janitor", # COlumn name handling
  "glue", # A better paste function
  "sweep", # Broom-style tidiers for the forecast package
  "recipes", # Carry transformation to new data,
  
  # Code styling
  "styler", # Style r code
  
  # Parallel processing
  "doParallel" # Allow parallel processing with caret
 
)

#set seed for reproducible results with h2o
set.seed(777)

# setwd("C:/Users/janne/Documents/time-series-forecast")
setwd(glue(getwd(), "/time-series-forecast"))

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
n <- 4

# Specify unit of measurement
unit.of.measurement <- "unit"

# Minimum number of unique instances turned into factors
factor.limit <- 12

# Preproceesing for multivariate models
preprocess <- c("center", "scale", "YeoJohnson")

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
    dplyr::select(scale)

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
    TidyAcf(unit, lags = max(n):max.lag) %>%
    filter(acf >= 0.6) %>%
    #filter(acf >= max( acf[acf != max(acf)] )) %>%  # max(acf)
    pull(lag)

  flog.info("Plotting ACF")
  AcfPlot(forecast.data.cleaned)
  
  flog.info("Creating timetk features")
  forecast.data.features <- CreateTimeTkFeatures(forecast.data.cleaned)
  
  #flog.info("Creating harmonic features features")
  #forecast.data.features <- HarmonicFeatures(forecast.data.features)
  
  flog.info("Cleaning features data")
  forecast.data.cleaned <- CleanFeatures(forecast.data.features)
  
  #flog.info("Creating sin and cos features")
  #forecast.data.cleaned <- SinCosFeatures(forecast.data.cleaned)

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
