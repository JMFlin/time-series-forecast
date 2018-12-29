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


CreateTimeTkFeatures <- function(forecast.data.cleaned) {
  flog.info("Augmenting data")
  forecast.data.augmented <- forecast.data.cleaned %>%
    tk_augment_timeseries_signature() %>%
    dplyr::select(-diff)

  flog.info("Inserting optimal lag into feature data")
  forecast.data.lagged <- bind_cols(forecast.data.augmented, forecast.data.augmented %>%
    select(unit) %>%
    nest() %>%
    mutate(lags = map(data, function(dat) {
      imap_dfc(dat, ~set_names(map(optimal.lag.setting, lag, x = .x), #1:2
                               paste0(.y, '_lag', optimal.lag.setting))) #1:2
    })) %>%
    unnest() %>%
    select(-unit))

  return(forecast.data.lagged)
}


CreateFutureData <- function(forecast.data.cleaned) {

  # Retrieves the timestamp information
  forecast.idx <- forecast.data.cleaned %>%
    tk_index()

  flog.info("Creating future time indexes")
  # Make future index
  new.data.tbl <- forecast.idx %>%
    tk_make_future_timeseries(n_future = max(n)) %>%
    tk_get_timeseries_signature() %>%
    slice((n() - (max(n))):n()) %>%
    mutate(date = index) %>%
    dplyr::select(-diff, -index)

  new.data.tbl <- bake(rec.obj, new.data.tbl) %>%
    clean_names()

  flog.info("Matching lags to correct future time indexes")
  forecast.data.forward <- forecast.data.cleaned %>%
    mutate(date = as.Date(floor_date(date + months(max(optimal.lag.setting), abbreviate = FALSE), unit = "month")))
  
  flog.info("Creating feature table")
  feature.data.tbl <- inner_join(new.data.tbl, forecast.data.forward %>% dplyr::select(date, contains("unit_"), 
                                                                                        contains("sin_lag"), 
                                                                                        contains("cos_lag")), by = c("date")) %>%
    clean_names() 
  
  if (any(names(forecast.data.cleaned) %in% "sin_2")) {
    ssp <- spectrum(forecast.data.cleaned$unit)
    per <- 1/ssp$freq[ssp$spec == max(ssp$spec)]
    t <- 1:max(n)
    
    feature.data.tbl <- bind_cols(feature.data.tbl, sin_2 = sin(2*pi/per*t),
                                  cos_2 = cos(2*pi/per*t),
                                  sin_4 = sin(4*pi/per*t),
                                  cos_4 = cos(4*pi/per*t))
  }
  

  feature.data.tbl <- feature.data.tbl %>%
    mutate_if(is.ordered, ~ as.character(.) %>% as.factor())

  return(feature.data.tbl)
}

HarmonicFeatures <- function(forecast.data.features) {
  
  features <- tk_ts(forecast.data.features %>% select(unit) %>% as_vector(), freq = seasonal.periods[1]) %>%
    harmonic(.) %>%
    as.tibble() %>% rename("cos" = !!names(.[1]),
                           "sin" = !!names(.[2])) 
  
  ccf.data.sin <- ccf(features %>% select(sin), forecast.data.features$unit, ylab = "cross-correlation", lag.max = max(optimal.lag.setting), plot = TRUE) %>%
    .$acf %>%
    .[, , 1] %>%
    as.tibble() %>%
    slice(1:(max(optimal.lag.setting) - max(n))) %>%
    mutate(index =  (max(optimal.lag.setting) - 1):max(n) + 1) %>% ##floor(max.lag/2)
    filter(abs(value) > 0.2)  %>%
      rename("sin" = !!names(.[1])) 
    
  ccf.data.cos <- ccf(features %>% select(cos), forecast.data.features$unit, ylab = "cross-correlation", lag.max = max(optimal.lag.setting), plot = TRUE) %>%
      .$acf %>%
      .[, , 1] %>%
      as.tibble() %>%
    slice(1:(max(optimal.lag.setting) - max(n))) %>%
    mutate(index =  (max(optimal.lag.setting) - 1):max(n) + 1) %>% ##floor(max.lag/2)
    filter(abs(value) >= 0.2) %>%
    rename("sin" = !!names(.[1]))
  
  drop <- names(features)
  
  sin.features <- features %>%
    select(sin) %>%
    nest() %>%
    mutate(lags = map(data, function(dat) {
      imap_dfc(dat, ~set_names(map(ccf.data.sin %>% select(index) %>% as_vector(), lag, x = .x), #1:2
                               paste0(.y, '_lag', ccf.data.sin$index))) #1:2
    })) %>%
    unnest()
  
  cos.features <- features %>%
    select(cos) %>%
    nest() %>%
    mutate(lags = map(data, function(dat) {
      imap_dfc(dat, ~set_names(map(ccf.data.cos %>% select(index) %>% as_vector(), lag, x = .x), #1:2
                               paste0(.y, '_lag', ccf.data.cos$index))) #1:2
    })) %>%
    unnest()
  
  forecast.data.features <- bind_cols(forecast.data.features, cos.features, sin.features) %>% 
    dplyr::select(-drop)
  
  return(forecast.data.features)
  
}


SinCosFeatures <- function(forecast.data.features) {
  
  detrended.trajectory <<- lm(unit ~ index(date), data = forecast.data.features)$residuals
  ssp <- spectrum(forecast.data.features$unit)
  per <- 1/ssp$freq[ssp$spec == max(ssp$spec)]
  t <- 1:(length(detrended.trajectory))
  
  reslm <- lm(detrended.trajectory ~ sin(2*pi/per*t) +
     cos(2*pi/per*t) +
     sin(4*pi/per*t) +
     cos(4*pi/per*t))
  
  detrended.trajectory %>%
    as.tibble() %>%
    mutate(fit = predict(reslm)) %>%
    ggplot(aes(x = 1:length(detrended.trajectory), y = value)) +
    geom_line(color = "red", size = 0.5) +
    geom_line(aes(x = 1:length(detrended.trajectory), y = fit),
              color = "dodgerblue", size = 0.5) +
    labs(x = "", 
         y = unit.of.measurement,
         title = "DFT analysis",
         subtitle = "DFT fit overlay") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5))

  
  forecast.data.features <- bind_cols(forecast.data.features, sin_2 = sin(2*pi/per*t),
                                         cos_2 = cos(2*pi/per*t),
                                         sin_4 = sin(4*pi/per*t),
                                         cos_4 = cos(4*pi/per*t))
  
  return(forecast.data.features)
}




