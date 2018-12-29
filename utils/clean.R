CleanTarget <- function(forecast.data, data.frequency) {
  forecast.data <- forecast.data %>%
    clean_names() %>%
    dplyr::select(date, unit) %>%
    arrange(date) %>%
    mutate(unit = na.approx(unit, maxgap = 5, rule = 2))

  full.ts <- seq(min(forecast.data$date), max(forecast.data$date), by = (data.frequency %>%
    as_data_frame() %>%
    as.character())) %>%
    as_data_frame() %>%
    rename("date" = value) %>%
    mutate(impute.unit = 0)

  forecast.data.full <- inner_join(forecast.data, full.ts, by = "date") %>%
    mutate(unit = unit + impute.unit) %>%
    dplyr::select(date, unit)

  return(forecast.data.full)
}

CleanFeatures <- function(forecast.data.features) {
  flog.info("Cleaning augmented data")
  forecast.data.cleaned <- forecast.data.features %>%
    clean_names() %>%
    remove_empty(c("cols")) %>%
    # select_if(~ !any(is.na(.))) %>%
    mutate_if(is.ordered, ~ as.character(.) %>% as.factor())

  flog.info("Finding near zero variance features")
  zero.variance.columns <- nearZeroVar(forecast.data.features, names = TRUE)

  flog.info("Removing near zero variance features")
  forecast.data.removed <- forecast.data.features %>%
    dplyr::select(-one_of(zero.variance.columns))

  # Numeric Factor Data
  unique.numeric.values.tbl <- forecast.data.removed %>%
    select_if(is.numeric) %>%
    map_df(~ unique(.) %>% length()) %>%
    gather() %>%
    arrange(value) %>%
    mutate(key = as_factor(key))

  num.2.factor.names <- unique.numeric.values.tbl %>%
    filter(value <= factor.limit) %>%
    arrange(desc(value)) %>%
    pull(key) %>%
    as.character()

  # Missing Data
  missing.tbl <- forecast.data.removed %>%
    summarize_all(.funs = ~ sum(is.na(.)) / length(.)) %>%
    gather() %>%
    arrange(desc(value)) %>%
    filter(value > 0)

  # Impute Data
  if (num.2.factor.names %>% is_empty()) {
    rec.obj <<- recipe(~., data = forecast.data.removed) %>%
      step_modeimpute(all_nominal()) %>%
      prep(stringsAsFactors = FALSE)
  } else {
    rec.obj <<- recipe(~., data = forecast.data.removed) %>%
      # step_string2factor(string.2.factor.names) %>%
      step_num2factor(num.2.factor.names) %>%
      # step_meanimpute(all_numeric()) %>%
      step_modeimpute(all_nominal()) %>%
      prep(stringsAsFactors = FALSE)
  }

  forecast.data.cleaned <- bake(rec.obj, forecast.data.removed) %>%
    clean_names() %>%
    mutate_if(is.ordered, ~ as.character(.) %>% as.factor()) %>%
    drop_na()

  return(forecast.data.cleaned)
}
