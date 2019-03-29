library(astsa)
library(forecast)

data("AirPassengers")

plot(AirPassengers)
acf(AirPassengers, lag.max = 100)

default <- auto.arima(AirPassengers, test = "adf", D = 1)

plot(diff(diff(AirPassengers, 1), 12))
acf(diff(diff(AirPassengers, 1), 12))

seas.tran <- auto.arima(AirPassengers, d = 1, D = 1, ic = "aic",
                   lambda = "auto")
seas <- auto.arima(AirPassengers, d = 1, D = 1, ic = "aic")

plot(seas$residuals)
plot(seas.tran$residuals)
