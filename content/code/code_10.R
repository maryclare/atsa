rm(list = ls())

library(astsa)
library(forecast)
library(rugarch)
library(stochvol)
library(coda)

data("nyse")
nyse <- nyse - mean(nyse)
n <- length(nyse)
to.use <- 1:(n - 300)
y <- nyse[to.use]

par(mfrow = c(1, 1))
plot(c(nyse), type = "l")

# These coefficients don't look well estimated, let's use a smaller model instead
# Fitting a GARCH(1, 0) model will also let us compare to Example 5.4 in S&S
spec10 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1, 0)),
                     mean.model=list(armaOrder=c(0, 0, 0), 
                                     # Note, these are ARFIMA parameter values not ARFIMA, 
                                     # always keep the middle value zero unless you learn later about
                                     # ARFIMA models later and want to fit them!
                                     include.mean=TRUE), distribution.model = "norm")
# Let's fit our simple GARCH Model
fit10 <- ugarchfit(data=y,spec=spec10)
n.ahead <- length((length(to.use) + 1):n)
forc <- ugarchforecast(fit10, n.ahead = n.ahead)

# Now lets simulate from the posterior of our simple stochastic volatility model
res <- svsample(y)
pred <- predict(res, n.ahead)
names(summary(res))


summary(res, showlatent=FALSE)

plot(c(nyse)^2, type = "l", col = "gray", xlim = c(length(nyse) - 2*n.ahead, length(nyse)), ylim = c(0, 0.0015))
lines(y^2, col = "black")
lines((length(to.use) + 1):n, sigma(forc)^2, col = "red")
lines((length(to.use) + 1):n, colMeans(exp(pred$h)), col = "blue")


plot(c(nyse), type = "l", col = "gray", xlim = c(length(nyse) - 2*n.ahead, length(nyse)), ylim = c(-1, 1)*0.06)
lines(y, col = "black")
lines((length(to.use) + 1):n, fitted(forc), col = "red")
lines((length(to.use) + 1):n, colMeans(pred$y), col = "blue")
lines((length(to.use) + 1):n, apply(pred$y, 2, function(x){quantile(x, 0.025)}), col = "blue", lty = 2)
lines((length(to.use) + 1):n, apply(pred$y, 2, function(x){quantile(x, 0.975)}), col = "blue", lty = 2)
