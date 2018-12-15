AcfPlot <- function(forecast.data.cleaned, optimal.lag.setting, max.lag) {
  forecast.data.cleaned %>%
    TidyAcf(unit, lags = 1:max.lag) %>%
    ggplot(aes(lag, acf)) +
    geom_vline(xintercept = optimal.lag.setting, size = 3, color = palette_light()[[2]]) +
    geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]], size = 2) +
    geom_label(aes(label = acf %>% round(2)),
      vjust = -1, color = palette_light()[[1]]
    ) +
    theme_tq() +
    labs(title = "ACF")
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
