AcfPlot <- function(forecast.data.cleaned) {
  acf <- forecast.data.cleaned %>%
    TidyAcf(unit, lags = 1:max.lag)

  max.ylim <- max(acf$acf)
  min.ylim <- min(acf$acf)

  ggplot(acf, aes(lag, acf)) +
    geom_vline(xintercept = optimal.lag.setting, size = 3, color = palette_light()[[2]]) +
    geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]], size = 2) +
    geom_label(aes(label = acf %>% round(2)),
      vjust = -1, color = palette_light()[[1]]
    ) +
    ylim(min(min.ylim) - 0.05, max(max.ylim) + 0.05) +
    theme_tq() +
    labs(title = "Optimal ACF")
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


TrueForecasts <- function(forecast.data, predictions.tbl, final.tbl) {
  predictions.tbl <- predictions.tbl %>%
    add_row(date = final.tbl$date, pred = final.tbl$pred)

  # Plot Beer Sales Forecast
  forecast.data %>%
    ggplot(aes(x = date, y = unit)) +
    # Training data
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    # Predictions
    geom_line(aes(y = pred), color = palette_light()[[2]], data = predictions.tbl) +
    geom_point(aes(y = pred), color = palette_light()[[2]], data = predictions.tbl) +
    # Aesthetics
    theme_tq() +
    labs(
      title = "Forecast: Time Series Machine Learning"
    )
}

ActualVsPredicted <- function(forecast.data, predictions.tbl) {
  actuals.tbl <- forecast.data %>%
    filter(predictions.tbl$date == forecast.data$date)

  # Plot Beer Sales Forecast
  forecast.data %>%
    ggplot(aes(x = date, y = unit)) +
    # Training data
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    # Actuals
    geom_line(color = palette_light()[[1]], data = actuals.tbl) +
    geom_point(color = palette_light()[[1]], data = actuals.tbl) +
    # Predictions
    geom_line(aes(y = pred), color = palette_light()[[2]], data = predictions.tbl) +
    geom_point(aes(y = pred), color = palette_light()[[2]], data = predictions.tbl) +
    # Aesthetics
    theme_tq() +
    labs(
      title = "Forecast: Time Series Machine Learning"
    )
}

TrainingStrategy <- function(forecast.data.lagged) {
  xmin.valid <- forecast.data.lagged %>%
    filter(forecast.data.lagged$date > (max(forecast.data.lagged$date) - years(1)) &
      forecast.data.lagged$date < (max(forecast.data.lagged$date) - months(6, abbreviate = FALSE))) %>%
    summarize(earliest_date = as.numeric(min(date)), latest_date = as.numeric(max(date))) %>%
    select(earliest_date, latest_date)

  xmin.train <- forecast.data.lagged %>%
    filter(forecast.data.lagged$date == (max(forecast.data.lagged$date) - months(6, abbreviate = FALSE) + months(1, abbreviate = FALSE))) %>%
    summarize(earliest_date = as.numeric(min(date))) %>%
    select(earliest_date)

  forecast.data.lagged %>%
    ggplot(aes(date, unit)) +
    # Validation Region
    geom_rect(
      xmin = xmin.valid$earliest_date,
      xmax = xmin.valid$latest_date,
      ymin = 0, ymax = Inf, alpha = 0.02,
      fill = palette_light()[[3]]
    ) +
    # Training Region
    geom_rect(
      xmin = xmin.train$earliest_date,
      xmax = xmin.valid$latest_date,
      ymin = 0, ymax = Inf, alpha = 0.02,
      fill = palette_light()[[4]]
    ) +
    # Data
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    # Aesthetics
    theme_tq() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      # title = "Beer Sales: 2007 through 2017",
      # subtitle = "Train, Validation, and Test Sets Shown"
    )
}
