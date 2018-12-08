Evaluate <- function(){

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
