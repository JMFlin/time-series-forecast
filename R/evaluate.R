Evaluate <- function(forecast.data, predictions.tbl){

  # Investigate test error
  error.tbl <- inner_join(forecast.data, predictions.tbl, by = "date") %>%
    #add_column(pred = predictions.tbl %>% as.tibble() %>% pull(pred)) %>%
    rename(actual = unit) %>%
    mutate(
      error     = actual - pred,
      error.pct = error / actual
    ) %>%
    summarise(
      me   = mean(error),
      rmse = mean(error^2)^0.5,
      mae  = mean(abs(error)),
      mape = mean(abs(error.pct)),
      mpe  = mean(error.pct)
    )

  return(error.tbl)
}

TrueForecasts <- function(forecast.data, predictions.tbl, final.tbl){

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
    labs(title = "Forecast: Time Series Machine Learning",
         subtitle = "Using basic multivariate linear regression can yield accurate results")

}

ActualVsPredicted <- function(forecast.data, predictions.tbl){

  actuals.tbl <- forecast.data %>%
    filter(predictions.tbl$date == forecast.data$date)

  # Plot Beer Sales Forecast
  forecast.data %>%
    ggplot(aes(x = date, y = unit)) +
    # Training data
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    # Predictions
    geom_line(aes(y = pred), color = palette_light()[[2]], data = predictions.tbl) +
    geom_point(aes(y = pred), color = palette_light()[[2]], data = predictions.tbl) +
    # Actuals
    geom_line(color = palette_light()[[1]], data = actuals.tbl) +
    geom_point(color = palette_light()[[1]], data = actuals.tbl) +
    # Aesthetics
    theme_tq() +
    labs(title = "Forecast: Time Series Machine Learning",
         subtitle = "Using basic multivariate linear regression can yield accurate results")

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

  # Plot Beer Sales with train, validation, and test sets shown
  forecast.data.lagged %>%
    ggplot(aes(date, unit)) +
    # Validation Region
    geom_rect(xmin = xmin.valid$earliest_date,
              xmax = xmin.valid$latest_date,
              ymin = 0, ymax = Inf, alpha = 0.02,
              fill = palette_light()[[3]]) +
    # Training Region
    geom_rect(xmin = xmin.train$earliest_date,
              xmax = xmin.valid$latest_date,
              ymin = 0, ymax = Inf, alpha = 0.02,
              fill = palette_light()[[4]]) +
    # Data
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    # Aesthetics
    theme_tq() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = "Beer Sales: 2007 through 2017",
         subtitle = "Train, Validation, and Test Sets Shown")

}
