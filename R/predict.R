ModelH2O <- function(forecast.data.lagged){

  tibble.list <- list()

  idx <- forecast.data.lagged %>%
    tk_index()

  for (i in 1:6) {

    flog.info(glue("Starting h2o modeling for ", as.character(max(idx) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE))))

    flog.info("Spliting into training, validation and test sets")
    # Split into training, validation and test sets
    train.tbl <- forecast.data.lagged %>%
      filter(forecast.data.lagged$date < (max(forecast.data.lagged$date) - years(1))) %>%
      select_if(~ !is.Date(.))

    valid.tbl <- forecast.data.lagged %>%
      filter(forecast.data.lagged$date > (max(forecast.data.lagged$date) - years(1)) &
               forecast.data.lagged$date < (max(forecast.data.lagged$date) - months(6, abbreviate = FALSE))) %>%
      select_if(~ !is.Date(.))

    test.tbl <- forecast.data.lagged %>%
      filter(forecast.data.lagged$date == (max(forecast.data.lagged$date) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE)))

    # Retrieves the timestamp information
    forecast.idx <- test.tbl %>%
      tk_index()

    test.tbl <- test.tbl %>%
      select_if(~ !is.Date(.))

    ##
    #remove near zero var cols
    ##
    flog.info("Converting to h2oframe objects")
    # Convert to H2OFrame objects
    train.h2o <- as.h2o(train.tbl)
    valid.h2o <- as.h2o(valid.tbl)
    test.h2o  <- as.h2o(test.tbl)

    x <- setdiff(names(train.h2o), "unit")

    flog.info("Starting h2o.automl")
    automl.models.h2o <- h2o.automl(
      x = x,
      y = "unit",
      training_frame = train.h2o,
      validation_frame = valid.h2o,
      leaderboard_frame = test.h2o,
      max_runtime_secs = 60,
      stopping_metric = "deviance")

    flog.info("Extract leader model")
    # Extract leader model
    automl.leader <- automl.models.h2o@leader

    # Make predictions
    pred.h2o <- h2o.predict(automl.leader, newdata = test.h2o)

    # Predictions with timestamps
    predictions.tbl <- tibble(
      date  = forecast.idx,
      pred = as.vector(pred.h2o)
    )

    flog.info("Appending predictions")
    # Append predictions
    tibble.list[[i]] <- predictions.tbl

  }

  flog.info("Collapsing tibbles")
  predictions.tbl <- bind_rows(tibble.list)

  return(predictions.tbl)
}

ModelLM <- function(forecast.data.lagged){

  tibble.list <- list()

  idx <- forecast.data.lagged %>%
    tk_index()

  for (i in 1:6) {

    flog.info(glue("Starting lm modeling for ", as.character(max(idx) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE))))

    flog.info("Spliting into training, validation and test sets")
    # Split into training, validation and test sets
    train.tbl <- forecast.data.lagged %>%
      filter(forecast.data.lagged$date < (max(forecast.data.lagged$date) - months(6, abbreviate = FALSE))) %>%
      select_if(~ !is.Date(.))

    test.tbl <- forecast.data.lagged %>%
      filter(forecast.data.lagged$date == (max(forecast.data.lagged$date) - months(6, abbreviate = FALSE) + months(i, abbreviate = FALSE)))

    # Retrieves the timestamp information
    forecast.idx <- test.tbl %>%
      tk_index()

    test.tbl <- test.tbl %>%
      select_if(~ !is.Date(.))

    ##
    #remove near zero var cols
    ##

    fit.lm <- lm(unit ~ ., data = train.tbl)

    # Make predictions
    pred <- predict(fit.lm, newdata = test.tbl)

    # Predictions with timestamps
    predictions.tbl <- tibble(
      date  = forecast.idx,
      pred = as.vector(pred)
    )

    flog.info("Appending predictions")
    # Append predictions
    tibble.list[[i]] <- predictions.tbl

  }

  flog.info("Collapsing tibbles")
  predictions.tbl <- bind_rows(tibble.list)

}