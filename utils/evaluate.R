Evaluate <- function(forecast.data, predictions.tbl) {

  # Investigate test error
  error.tbl <- inner_join(forecast.data, predictions.tbl, by = "date") %>%
    # add_column(pred = predictions.tbl %>% as.tibble() %>% pull(pred)) %>%
    rename(actual = unit) %>%
    mutate(
      error = actual - pred,
      error.pct = error / actual
    ) %>%
    summarise(
      me = mean(error),
      rmse = mean(error^2)^0.5,
      mae = mean(abs(error)),
      mape = mean(abs(error.pct)),
      mpe = mean(error.pct)
    )

  return(error.tbl)
}

