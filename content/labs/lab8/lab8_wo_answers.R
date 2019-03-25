###################################
# Author: Megan Gelsinger (Maryclare Griffin)
# Date: 3/22/19
# Title: STSCI 4550 Lab Session 8 
###################################

# In this lab demo, we are going to see what happens when we over difference a time series.
# We are also going to perform ARIMA and SARIMA analyses on real data.

## Over Differencing ##

# It might surprise you that we can in fact over difference a time series.  
# A visual makes this easiest to see. 

# Let's work with a (stationary) white noise process. We are going to look at d = 0, 1, and 4.
library(forecast)
set.seed(1234)
x <- rnorm(100)
diff_x <- diff(x)
diff4_x <- diff(x, differences = 4)
par(mfrow = c(3, 1))
plot(x, type = "l", main = "x"); plot(diff_x, type = "l", main = "diff_x"); plot(diff4_x, type = "l", main = "diff4_x")

# Was differencing necessary? 
binned.mv.plot <- function(x, bin.size = 10) {
  
  bins <- floor(0:(length(x) - 1)/bin.size) 
  small.bins <- which(table(bins) == min(table(bins)) & !table(bins) == max(table(bins)))
  bins[bins == small.bins - 1] <- NA
  
  bin.means <- aggregate(as.numeric(x), list("bins" = bins), mean)$x
  bin.vars <- aggregate(as.numeric(x), list("bins" = bins), var)$x
  
  par(mfrow = c(1, 2))
  plot(bin.means, type = "b", xlab = "Bin", ylab = "Mean", ylim = range(x))
  plot(bin.vars, type = "b", xlab = "Bin", ylab = "Variance", ylim = c(0, max(bin.vars)+10))
}
binned.mv.plot(x)
ndiffs(x, alpha = 0.05, test = "adf", type = "level")

# Let's pick models for each time series and perform some forecasting.  We will quickly see the problem with over differencing.
# Note: could also use auto.arima() to select model with lowest AIC/BIC but use with caution.
n.ahead <- 10
forecast_x <- forecast(arima(x, order = c(0, 0, 1)), h = n.ahead) 
forecast_dx <- forecast(arima(x, order = c(0, 1, 1)), h = n.ahead)
forecast_d4x <- forecast(arima(x, order = c(0, 4, 1)), h = n.ahead)
max_upper <- max(max(forecast_x$upper), max(forecast_dx$upper), max(forecast_d4x$upper))
min_lower <- min(min(forecast_x$lower), min(forecast_dx$lower), min(forecast_d4x$lower))

par(mfrow = c(3, 1))
plot(forecast_x, ylim = c(min_lower, max_upper));plot(forecast_dx, ylim = c(min_lower, max_upper));plot(forecast_d4x, ylim = c(min_lower, max_upper))
plot(forecast_x); plot(forecast_dx);plot(forecast_d4x)

# Why do you think we see the behavior that we do in the above plots?


## ARIMA/SARIMA ##

# We will now go through the process of fitting and forecasting ARIMA and SARIMA models using real data.
# We are going to follow these steps:
#   1. Plot the (raw) data
#   2. Assess potential sources of non-stationarity 
#   3. Address non-stationarity in mean/variance (and verify)
#   4. Assess potential seasonality
#   5. Address seasonality (and verify)
#   6. Fit data to appropriate model

# Example 1: fdeaths (Monthly Deaths from Lung Diseases in the UK)
# 1.
par(mfrow = c(1, 1))
plot(fdeaths)

# 2. What are potential sources of non-stationarity, if any?
binned.mv.plot <- function(x, bin.size = 6) {
  
  bins <- floor(0:(length(x) - 1)/bin.size) 
  small.bins <- which(table(bins) == min(table(bins)) & !table(bins) == max(table(bins)))
  bins[bins == small.bins - 1] <- NA
  
  bin.means <- aggregate(as.numeric(x), list("bins" = bins), mean)$x
  bin.vars <- aggregate(as.numeric(x), list("bins" = bins), var)$x
  
  par(mfrow = c(1, 2))
  plot(bin.means, type = "b", xlab = "Bin", ylab = "Mean", ylim = range(x))
  plot(bin.vars, type = "b", xlab = "Bin", ylab = "Variance", ylim = c(0, max(bin.vars)+2))
}
binned.mv.plot(fdeaths)
ndiffs(fdeaths, alpha = 0.05, test = "adf", type = "level")

# 3. How do we address this/these sources of non-stationarity, if there are any? 
l_fdeaths <- log(fdeaths)
binned.mv.plot(l_fdeaths)

# 4. Do we suspect any seasonal effects?
# One way to check visually - are there obvious cycles that correspond with the seasonal frequency?
# Another way to check visually - look at season plot and/or for significance around order of seasonality in ACF plots 
par(mfrow = c(1, 1))
plot(l_fdeaths) 
monthplot(l_fdeaths)
par(mfrow = c(2, 1))
acf(l_fdeaths);pacf(l_fdeaths)

# 5. How do we address this seasonality, if present?
dsl_fdeaths <- diff(l_fdeaths, lag = 12)
par(mfrow = c(1, 1))
plot(dsl_fdeaths)
par(mfrow = c(2, 1))
acf(dsl_fdeaths); pacf(dsl_fdeaths) 

# 6. Fit an appropriate model
# Based on what we saw in 1 - 5, we have several models that we can try. 
# SARIMA(0, 0, 0)(0, 1, 0) 
model1 <- arima(l_fdeaths, seasonal = list(order = c(0, 1, 0), period = 12),
                xreg = time(fdeaths))
# SARIMA(0, 0, 0)(1, 1, 0) 
model2 <- arima(l_fdeaths, seasonal = list(order = c(1, 1, 0), period = 12),
                xreg = time(fdeaths))
# SARIMA(0, 0, 0)(0, 1, 1)
model3 <- arima(l_fdeaths, seasonal = list(order = c(0, 1, 1), period = 12),
                xreg = time(fdeaths))

# Which model is best?
# AIC
AIC(model1); AIC(model2); AIC(model3) 
# BIC
AIC(model1, k = log(length(model1$nobs))); AIC(model2, k = log(length(model2$nobs))); AIC(model3, k = log(length(model3$nobs))) 

# Let's do some forecasting quick 
par(mfrow = c(1, 1))
plot(forecast(model1, h = 4, xreg = (max(time(fdeaths)) + (1:4)/12)))
# Sanity check: What does the "4" represent above? What does the "12" represent?
# Keep in mind downfalls of transforming data! Look at your notes - what might be true about these forecasts
# on the original scale of the data, when we back-transform?


# Example 2: austres (Quarterly Time Series of the Number of Australian Residents) #
# 1.
par(mfrow = c(1, 1))
plot(austres)

# 2. What are potential sources of non-stationarity, if any?
binned.mv.plot(austres)
ndiffs(austres, alpha = 0.05, test = "adf", type = "level")

# 3. How do we address this/these sources of non-stationarity, if there are any? 
dl_austres <- diff(log(austres))
par(mfrow = c(1, 1))
plot(dl_austres)
binned.mv.plot(dl_austres)

# 4. Do we suspect any seasonal effects?
par(mfrow = c(1, 1))
plot(dl_austres)
par(mfrow = c(2, 1))
acf(dl_austres); pacf(dl_austres)

# 5. How do we address this seasonality, if present?
dsdl_austres <- diff(dl_austres, lag = 4)
par(mfrow = c(1, 1))
plot(dsdl_austres)
par(mfrow = c(2, 1))
acf(dsdl_austres); pacf(dsdl_austres) 

# 6. Fit an appropriate model
# Based on what we saw in 1 - 5, we have several models that we can try.  All of our models will have 
# one regular differencing.  We are still unsure whether or not we need to address seasonality.
# ARIMA(0, 1, 0)
model1 <- arima(log(austres), order = c(0, 1, 0), xreg = time(austres))
# ARIMA(1, 1, 0)
model2 <- arima(log(austres), order = c(1, 1, 0), xreg = time(austres))
# SARIMA(1, 1, 0)(0, 1, 0) 
model3 <- arima(log(austres), order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 4),
                xreg = (time(austres))^2)
# SARIMA(1, 1, 0)(1, 1, 0)
model4 <- arima(log(austres), order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 4),
                xreg = (time(austres))^2)
# SARIMA(1, 1, 0)(2, 1, 0)
model5 <- arima(log(austres), order = c(1, 1, 0), seasonal = list(order = c(2, 1, 0), period = 4),
                xreg = (time(austres))^2)

# Which model is best?
# AIC
AIC(model1); AIC(model2) 
AIC(model3); AIC(model4); AIC(model5) 
# Why did I break the models into the two groups ahead?  Why didn't I compare all of them together?
# BIC
AIC(model1, k = log(length(model1$nobs))); AIC(model2, k = log(length(model2$nobs))) 
AIC(model3, k = log(length(model3$nobs))); AIC(model4, k = log(length(model4$nobs)));  AIC(model5, k = log(length(model5$nobs))) 

# Let's do some forecasting quick
par(mfrow = c(1, 1))
plot(forecast(model3, h = 12, xreg = (max(time(austres)) + (1:12)/4)^2))
# Sanity check: What does the "12" represent above? What does the "4" represent?

