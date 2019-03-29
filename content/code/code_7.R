library(astsa)
library(forecast)
library(fracdiff)

data("varve")
n <- length(varve)

plot(varve)
lvarv <- log(varve) - mean(log(varve))

plot(lvarv)

lag.max <- n - 1
acf(lvarv, lag.max = lag.max)
best.arma <- auto.arima(lvarv, 
                         d = 0, 
                         D = 0, 
                         max.P = 0, 
                         max.Q = 0, 
                         allowdrift = FALSE, 
                         allowmean=FALSE,
                         ic = "aic")

lines(ARMAacf(ar = coef(best.arma)[grepl("ar", names(coef(best.arma)))],
        ma = coef(best.arma)[grepl("ma", names(coef(best.arma)))],
        lag.max = lag.max), col = "red",
      lwd = 4)

plot(lvarv)
acf(lvarv, lag.max = lag.max)
best.ar <- auto.arima(lvarv, 
                         d = 0, 
                         D = 0, 
                         max.q = 0,
                         max.P = 0, 
                         max.Q = 0, 
                         allowdrift = FALSE, 
                         allowmean=FALSE,
                         ic = "aic")
lines(ARMAacf(ar = coef(best.ar)[grepl("ar", names(coef(best.ar)))],
              lag.max = lag.max), col = "red")

best.match <- acf2AR(acf(lvarv, lag.max = lag.max, plot = FALSE)$acf)[lag.max, ]
lines(ARMAacf(ar = best.match), col = "green")

hist(best.match, breaks = 50)

thresh <- 0.01
acf(lvarv, lag.max = lag.max)
lines(ARMAacf(ar = best.match), col = "green")
lines(ARMAacf(ar = sign(best.match)*pmax(thresh, abs(best.match))), col = "purple")
sum(abs(best.match) > thresh)

acf(lvarv, lag.max = lag.max)
lines(ARMAacf(ar = coef(best.arma)[grepl("ar", names(coef(best.arma)))],
              ma = coef(best.arma)[grepl("ma", names(coef(best.arma)))],
              lag.max = lag.max), col = "red")

fd.fit <- fracdiff(lvarv)
fd.sim <- fracdiff.sim(n = 50000, d = fd.fit$d)
lines(acf(fd.sim$series, lag.max = lag.max, plot = FALSE)$acf, col = "orange")

