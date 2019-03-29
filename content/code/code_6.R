library(astsa)
library(forecast)
library(rugarch)

data("nyse")
nyse <- nyse

par(mfrow = c(1, 1))
plot(nyse)
acf(nyse^2)
pacf(nyse^2)

# Let's take the ad-hoc approach of fitting an ARIMA model to the squared values
# and picking the best model using BIC 
ax2 <- auto.arima(nyse^2, d = 0, D = 0, max.P = 0, max.Q = 0, allowdrift = FALSE, allowmean=TRUE,
                  ic = "bic")
# This suggests a pretty big GARCH(3, 3) model.
# First we have to specify it
spec33 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3, 3)),
                 mean.model=list(armaOrder=c(0, 0, 0), 
                                 # Note, these are ARFIMA parameter values not ARIMA, 
                                 # always keep the middle value zero unless you learn later about
                                 # ARFIMA models later and want to fit them!
                                 include.mean=TRUE), distribution.model = "norm")
# Then we can fit it! omega is our alpha0 from class
fit33 <- ugarchfit(data=nyse,spec=spec33)

# These coefficients don't look well estimated, let's use a smaller model instead
# Fitting a GARCH(1, 1) model will also let us compare to Example 5.4 in S&S
spec11 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1, 1)),
                     mean.model=list(armaOrder=c(0, 0, 0), 
                                     # Note, these are ARFIMA parameter values not ARFIMA, 
                                     # always keep the middle value zero unless you learn later about
                                     # ARFIMA models later and want to fit them!
                                     include.mean=TRUE), distribution.model = "norm")
# Then we can fit it
fit11 <- ugarchfit(data=nyse,spec=spec11)

