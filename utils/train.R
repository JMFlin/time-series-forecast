ModelH2O <- function(forecast.data.cleaned) {
  tibble.list <- list()

  for (i in 1:6) {
    flog.info("Splitting data into train, validation and test sets")
    train.tbl <- forecast.data.cleaned %>%
      filter(forecast.data.cleaned$date < (max(forecast.data.cleaned$date) - years(1)) + months(i - 1, abbreviate = FALSE))

    valid.tbl <- forecast.data.cleaned %>%
      filter(forecast.data.cleaned$date >= (max(forecast.data.cleaned$date) - years(1)) &
        forecast.data.cleaned$date <= (max(forecast.data.cleaned$date) - months(6, abbreviate = FALSE) + months(i - 1, abbreviate = FALSE)))

    test.tbl <- forecast.data.cleaned %>%
      filter(forecast.data.cleaned$date == (max(forecast.data.cleaned$date) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE)))

    min.train.date <- min(as.Date(train.tbl$date))
    max.train.date <- max(as.Date(train.tbl$date))

    min.valid.date <- min(as.Date(valid.tbl$date))
    max.valid.date <- max(as.Date(valid.tbl$date))

    min.test.date <- min(as.Date(test.tbl$date))
    max.test.date <- max(as.Date(test.tbl$date))

    flog.info(glue(
      "Training window: {min.train.date} - {max.train.date} "
    ))

    flog.info(glue(
      "Validation window: {min.valid.date} - {max.valid.date} "
    ))

    flog.info(glue(
      "Testing window: {min.test.date} - {max.test.date} "
    ))

    # Retrieves the timestamp information
    forecast.idx <- test.tbl %>%
      tk_index()

    train.tbl <- train.tbl %>%
      select_if(~ !is.Date(.))

    valid.tbl <- valid.tbl %>%
      select_if(~ !is.Date(.))

    test.tbl <- test.tbl %>%
      select_if(~ !is.Date(.))

    flog.info("Converting to h2oframe objects")
    train.h2o <- as.h2o(train.tbl)
    valid.h2o <- as.h2o(valid.tbl)
    test.h2o <- as.h2o(test.tbl)

    x <- setdiff(names(train.h2o), "unit")

    flog.info("Starting h2o.automl training")
    automl.models.h2o <- h2o.automl(
      x = x,
      y = "unit",
      training_frame = train.h2o,
      validation_frame = valid.h2o,
      leaderboard_frame = test.h2o,
      max_runtime_secs = 20,
      stopping_metric = "deviance"
    )

    flog.info("Extracting leader model")
    # Extract leader model
    automl.leader <- automl.models.h2o@leader

    # Make predictions
    pred.h2o <- h2o.predict(automl.leader, newdata = test.h2o)

    # Predictions with timestamps
    predictions.tbl <- tibble(
      date = forecast.idx,
      pred = as.vector(pred.h2o)
    )

    flog.info("Appending predictions")
    # Append predictions
    tibble.list[[i]] <- predictions.tbl

    flog.info(glue("End of round {i}"))
    flog.info("=================================================")
  }

  flog.info("Collapsing tibbles")
  predictions.tbl.h2o <- bind_rows(tibble.list)

  H2O.model <- list(
    predictions.tbl.h2o = predictions.tbl.h2o,
    automl.leader = automl.leader
  )

  return(H2O.model)
}

ModelLM <- function(forecast.data.cleaned) {
  tibble.list <- list()

  for (i in 1:6) {
    flog.info("Splitting data into train and test sets")
    train.tbl <- forecast.data.cleaned %>%
      filter(date <= (max(date) - months(6, abbreviate = FALSE) + months(i - 1, abbreviate = FALSE)))

    test.tbl <- forecast.data.cleaned %>%
      filter(date == (max(date) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE)))

    min.train.date <- min(as.Date(train.tbl$date))
    max.train.date <- max(as.Date(train.tbl$date))

    min.test.date <- min(as.Date(test.tbl$date))
    max.test.date <- max(as.Date(test.tbl$date))

    flog.info(glue(
      "Training window: {min.train.date} - {max.train.date} "
    ))

    flog.info(glue(
      "Testing window: {min.test.date} - {max.test.date} "
    ))

    # Retrieves the timestamp information
    forecast.idx <- test.tbl %>%
      tk_index()

    train.tbl <- train.tbl %>%
      select_if(~ !is.Date(.))

    test.tbl <- test.tbl %>%
      select_if(~ !is.Date(.))

    flog.info("Starting lm training")
    fit.lm <- train(unit ~ .,
      data = train.tbl,
      method = "lm",
      trControl = regression.control,
      tuneGrid = expand.grid(intercept = FALSE)
    )


    # fit.lm <- lm(unit ~ ., data = train.tbl)

    # Make predictions
    pred <- predict(fit.lm, newdata = test.tbl)

    # Predictions with timestamps
    predictions.tbl <- tibble(
      date = forecast.idx,
      pred = as.vector(pred)
    )

    flog.info("Appending predictions")
    # Append predictions
    tibble.list[[i]] <- predictions.tbl

    flog.info(glue("End of round {i}"))
    flog.info("=================================================")
  }

  flog.info("Collapsing tibbles")
  predictions.tbl.lm <- bind_rows(tibble.list)

  LM.model <- list(
    predictions.tbl.lm = predictions.tbl.lm,
    fit.lm = fit.lm
  )

  return(LM.model)
}


ModelUnivariate <- function(forecast.data, data.frequency) {
  tibble.list <- list()

  for (i in 1:6) {
    flog.info("Splitting data into train and test sets")

    train.tbl <- forecast.data %>%
      filter(date <= (max(date) - months(6, abbreviate = FALSE) + months(i - 1, abbreviate = FALSE)))

    test.tbl <- forecast.data %>%
      filter(date == (max(date) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE)))

    flog.info("Converting to ts objects")
    train.ts <- tk_ts(train.tbl$unit,
      start = as.yearmon(glue(year(min(train.tbl$date)), "-0", month(min(train.tbl$date)))),
      frequency = data.frequency
    )

    test.ts <- tk_ts(test.tbl$unit,
      start = as.yearmon(glue(year(min(test.tbl$date)), "-0", month(min(test.tbl$date)))),
      frequency = data.frequency
    )

    min.train.date <- min(as.Date(train.tbl$date))
    max.train.date <- max(as.Date(train.tbl$date))

    min.test.date <- min(as.Date(test.tbl$date))
    max.test.date <- max(as.Date(test.tbl$date))

    flog.info(glue(
      "Training window: {min.train.date} - {max.train.date} "
    ))

    flog.info(glue(
      "Testing window: {min.test.date} - {max.test.date} "
    ))

    # Retrieves the timestamp information
    forecast.idx <- test.tbl %>%
      tk_index()

    flog.info("Starting training")
    models.list <- list(
      auto.arima = list(
        y = train.ts,
        max.p = optimal.lag.setting
      ),
      ets = list(
        y = train.ts
      ),
      tbats = list(
        y = train.ts
      )
    )

    models.tbl <- enframe(models.list, name = "model_names", value = "params")

    models.tbl.fit <- models.tbl %>%
      mutate(fit = invoke_map(model_names, params))

    models.tbl.fit %>%
      mutate(tidy = map(fit, sw_tidy)) %>%
      unnest(tidy) %>%
      spread(key = model_names, value = estimate)

    flog.info("Select best model")
    best.model <- models.tbl.fit %>%
      mutate(glance = map(fit, sw_glance)) %>%
      unnest(glance, .drop = TRUE) %>%
      arrange(desc(AIC)) %>%
      filter(row_number() == n())

    # models.tbl.fit %>%
    #   mutate(augment = map(fit, sw_augment, rename_index = "date")) %>%
    #   unnest(augment) %>%
    #   ggplot(aes(x = date, y = .resid, group = model_names)) +
    #   geom_line(color = palette_light()[[2]]) +
    #   geom_point(color = palette_light()[[1]]) +
    #   geom_smooth(method = "loess") +
    #   facet_wrap(~ model_names, nrow = 3) +
    #   labs(title = "Residuals Plot") +
    #   theme_tq()

    flog.info("Predicting with best model")
    models.tbl.fcast <- models.tbl.fit %>%
      filter(model_names == (best.model %>% select(model_names) %>% as_vector())) %>%
      mutate(fcast = map(fit, forecast, h = 1))

    models.tbl.fcast.tidy <- models.tbl.fcast %>%
      mutate(sweep = map(fcast, sw_sweep, fitted = FALSE, timetk_idx = TRUE, rename_index = "date"))

    # Predictions with timestamps
    predictions.tbl <- tibble(
      date = forecast.idx,
      pred = models.tbl.fcast.tidy %>%
        unnest(sweep) %>%
        filter(row_number() == n()) %>%
        select(value) %>%
        as_vector()
    )

    flog.info("Appending predictions")
    # Append predictions
    tibble.list[[i]] <- predictions.tbl

    flog.info(glue("End of round {i}"))
    flog.info("=================================================")
  }

  flog.info("Collapsing tibbles")
  predictions.tbl.uni <- bind_rows(tibble.list)

  TS.model <- list(
    predictions.tbl.uni = predictions.tbl.uni,
    fit.uni = models.tbl.fit %>%
      filter(model_names == (best.model %>% select(model_names) %>% as_vector()))
  )

  return(TS.model)
}


ModelProphet <- function(forecast.data.cleaned, data.frequency) {

  tibble.list <- list()

  for (i in 1:6) {
    flog.info("Splitting data into train and test sets")

    train.tbl <- forecast.data %>%
      filter(date <= (max(date) - months(6, abbreviate = FALSE) + months(i - 1, abbreviate = FALSE)))

    test.tbl <- forecast.data %>%
      filter(date == (max(date) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE)))

    min.train.date <- min(as.Date(train.tbl$date))
    max.train.date <- max(as.Date(train.tbl$date))

    min.test.date <- min(as.Date(test.tbl$date))
    max.test.date <- max(as.Date(test.tbl$date))

    train.tbl <- train.tbl %>%
      rename(ds = date,
             y = unit)

    test.tbl <- test.tbl %>%
      rename(ds = date,
             y = unit)

    flog.info(glue(
      "Training window: {min.train.date} - {max.train.date} "
    ))

    flog.info(glue(
      "Testing window: {min.test.date} - {max.test.date} "
    ))

    # Retrieves the timestamp information
    forecast.idx <- test.tbl %>%
      tk_index()

    prophet.model <- prophet(train.tbl,
                             growth = "linear", # growth curve trend
                             #n.changepoints = 100, # Prophet automatically detects changes in trends by selecting changepoints from the data
                             yearly.seasonality = TRUE, # yearly seasonal component using Fourier series
                             weekly.seasonality = TRUE # weekly seasonal component using dummy variables
                             #holidays = off_days
    )

    pred.prophet <- predict(prophet.model, test.tbl)

    # Predictions with timestamps
    predictions.tbl <- tibble(
      date = forecast.idx,
      pred = pred.prophet %>% select(yhat) %>% as_vector()
    )

    flog.info("Appending predictions")
    # Append predictions
    tibble.list[[i]] <- predictions.tbl

    flog.info(glue("End of round {i}"))
    flog.info("=================================================")
  }

  flog.info("Collapsing tibbles")
  predictions.tbl.uni <- bind_rows(tibble.list)

  PROPHET.model <- list(
    predictions.tbl.uni = predictions.tbl.uni,
    fit.prophet = prophet.model
  )

  return(PROPHET.model)
}
