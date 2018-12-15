LoadData <- function(unit) {
  forecast.data <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2017-12-31")

  forecast.data <- forecast.data %>%
    rename(!!unit := price)

  return(forecast.data)
}