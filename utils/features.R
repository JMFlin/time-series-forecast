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
    tk_make_future_timeseries(n_future = max(n)) %>%
    tk_get_timeseries_signature() %>%
    slice((n() - (max(n))):n()) %>%
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