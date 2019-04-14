###################################
# Author: Megan Gelsinger 
# Date: 4/12/19
# Title: STSCI 4550 Lab Session 9 
###################################

# In this lab demo, we are going to review the basics of fitting state-space models.
# We are also going to fit state-space models to time series with missing data (forecasting).  

## Basics - Function and Arguments ##
# We are going to use the "MARSS" package to fit our state-space models.  Let's read some documentation
# on the main function, "MARSS," to understand how it works. 
# install.packages("MARSS")
library(MARSS)
?MARSS 

# There are a lot of important arguments here, so let's break them down one at a time.
# y - data, time series, can have missing values

# model - what does our state-space model look like?
# Use the documentation to make sense of these arguments. 
model <- list( 
  B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), # pass parameter names want to see in output summaries
  Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
  x0=matrix("mu"), tinitx=0 )

# inits - what should be our starting parameter estimates?
# default: inits = list(B=1, U=0, Q=0.05, Z=1, A=0, R=0.05, x0=-99, V0=0.05, G=0, H=0, L=0, C=0, D=0, c=0, d=0)
# can pass more informative starting values...will see later in this lab 

# miss.values - make sure missing values in data are "NA"

# method - how should we estimate the parameter values?
# option 1: "kem", EM algorithm (discussed in class)
# option 2: "BFGS", BFGS algorithm (from residuals method discussed in class)
# which to use? run EM, if fails to converge use as starting values of BFGS 

## Basics - Fitting ##
# y - 
library(astsa)

data("varve")
n <- length(varve)

y <- log(varve)

# model - 
model <- list( 
  B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
  Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
  x0=matrix("mu"), tinitx=0 )

# fit the model - 
MARSS(y, model = model, method = "kem") 
fit_kem <- MARSS(c(y), model = model, method = "kem")
fit_bfgs <- MARSS(c(y), model = model, method = "BFGS")
fit_bfgs_with_kem <- MARSS(c(y), model = model, method = "BFGS", inits = fit_kem)
# How do the estimates compare?
cbind("kem" = fit_kem$coef, "BFGS" = fit_bfgs$coef, "kem-BFGS" = fit_bfgs_with_kem$coef)
# For this class - start with default (EM) and if doesn't converge, use as starting values in BFGS

# Get Kalman filter, predictor, and smoother
?MARSSkfss
kf <- MARSSkfss(fit_kem)

# Plot to filter, predictions, and smoother
par(mfrow = c(3, 1))
par(mar = rep(2, 4))
sub <- 1:50

plot(c(y)[sub], 
     main = "Kalman Predictions", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf[["xtt1"]]), col = "red", lwd = 2)
lines(c(kf[["xtt1"]]) - c(sqrt(kf[["Vtt1"]])), col = "red", lwd = 2, lty = 3)
lines(c(kf[["xtt1"]]) + c(sqrt(kf[["Vtt1"]])), col = "red", lwd = 2, lty = 3)

plot(c(y)[sub],
     main = "Kalman Filter", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf[["xtt"]]), col = "purple", lwd = 2)
lines(c(kf[["xtt"]]) - c(sqrt(kf[["Vtt"]])), col = "purple", lwd = 2, lty = 3)
lines(c(kf[["xtt"]]) + c(sqrt(kf[["Vtt"]])), col = "purple", lwd = 2, lty = 3)

plot(c(y)[sub],
     main = "Kalman Smoother", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf[["xtT"]]), col = "blue", lwd = 2)
lines(c(kf[["xtT"]]) - c(sqrt(kf[["VtT"]])), col = "blue", lwd = 2, lty = 3)
lines(c(kf[["xtT"]]) + c(sqrt(kf[["VtT"]])), col = "blue", lwd = 2, lty = 3)


## Basics - Change Parameter Values (Hint: HW) ##
# Let's recall the estimated parameter values.
est_coefs <- fit_kem$coef

# What if we want to see the effect of changing one of the parameter values?
# a = 1.5
model_newa <- list( 
  B=matrix(est_coefs[3]), U=matrix(0), Q=matrix(est_coefs[4]), # pass parameter names want to see in output summaries
  Z=matrix(1.5), A=matrix(0), R=matrix(est_coefs[2]),
  x0=matrix(est_coefs[5]), tinitx=0 )

# Now, re-fit the Kalman filter, predictor, and smoother
fit_newa <- MARSS(c(y), model = model_newa, method = "kem")
kf_newa <- MARSSkfss(fit_newa)

# Can we see differences?
par(mfrow = c(2, 3))
par(mar = rep(2, 4))
sub <- 1:50

plot(c(y)[sub], 
     main = "Kalman Predictions", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf[["xtt1"]]), col = "red", lwd = 2)
lines(c(kf[["xtt1"]]) - c(sqrt(kf[["Vtt1"]])), col = "red", lwd = 2, lty = 3)
lines(c(kf[["xtt1"]]) + c(sqrt(kf[["Vtt1"]])), col = "red", lwd = 2, lty = 3)

plot(c(y)[sub],
     main = "Kalman Filter", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf[["xtt"]]), col = "purple", lwd = 2)
lines(c(kf[["xtt"]]) - c(sqrt(kf[["Vtt"]])), col = "purple", lwd = 2, lty = 3)
lines(c(kf[["xtt"]]) + c(sqrt(kf[["Vtt"]])), col = "purple", lwd = 2, lty = 3)

plot(c(y)[sub],
     main = "Kalman Smoother", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf[["xtT"]]), col = "blue", lwd = 2)
lines(c(kf[["xtT"]]) - c(sqrt(kf[["VtT"]])), col = "blue", lwd = 2, lty = 3)
lines(c(kf[["xtT"]]) + c(sqrt(kf[["VtT"]])), col = "blue", lwd = 2, lty = 3)

plot(c(y)[sub], 
     main = "Kalman Predictions (New a)", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf_newa[["xtt1"]]), col = "red", lwd = 2)
lines(c(kf_newa[["xtt1"]]) - c(sqrt(kf_newa[["Vtt1"]])), col = "red", lwd = 2, lty = 3)
lines(c(kf_newa[["xtt1"]]) + c(sqrt(kf_newa[["Vtt1"]])), col = "red", lwd = 2, lty = 3)

plot(c(y)[sub],
     main = "Kalman Filter (New a)", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf_newa[["xtt"]]), col = "purple", lwd = 2)
lines(c(kf_newa[["xtt"]]) - c(sqrt(kf_newa[["Vtt"]])), col = "purple", lwd = 2, lty = 3)
lines(c(kf_newa[["xtt"]]) + c(sqrt(kf_newa[["Vtt"]])), col = "purple", lwd = 2, lty = 3)

plot(c(y)[sub],
     main = "Kalman Smoother (New a)", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf_newa[["xtT"]]), col = "blue", lwd = 2)
lines(c(kf_newa[["xtT"]]) - c(sqrt(kf_newa[["VtT"]])), col = "blue", lwd = 2, lty = 3)
lines(c(kf_newa[["xtT"]]) + c(sqrt(kf_newa[["VtT"]])), col = "blue", lwd = 2, lty = 3)


## Missing Values (at End of Time Series) for Forecasting ##
# We have covered how to get predictions, filtered and smoothed values.  What about getting forecasts?
# To make forecasts, we need to introduce 'NA' values at the end of our time series and fit the 
# state-space model. 

y_for <- c(y, rep(NA, 50))
n <- length(y_for)
fit_kem_y_for <- MARSS(c(y_for), model = model, method = "kem")

par(mfrow = c(1, 1))
plot(y_for, type = "l", col = "darkgray", main = "Forecasts", xlab = "Time", ylab = "y",
     xlim = c(0, n))
# Go back to your notes to see why the values used to generate the lines make sense!
lines((n - 50 + 1):n, (coef(fit_kem_y_for)$Z[1, 1]*c(fit_kem_y_for$states))[(n - 50 + 1):n], col = "purple", lwd = 2)
lines((n - 50 + 1):n, ((c(coef(fit_kem_y_for)$Z[1, 1]*fit_kem_y_for$states) - qnorm(0.975)*c(sqrt(coef(fit_kem_y_for)$Z[1, 1]^2*fit_kem_y_for$states.se^2 + coef(fit_kem_y_for)$R[1, 1]))))[(n - 50 + 1):n], col = "purple", lwd = 2, lty = 3)
lines((n - 50 + 1):n, (coef(fit_kem_y_for)$Z[1, 1]*(c(fit_kem_y_for$states) + qnorm(0.975)*c(sqrt(coef(fit_kem_y_for)$Z[1, 1]^2*fit_kem_y_for$states.se^2 + coef(fit_kem_y_for)$R[1, 1]))))[(n - 50 + 1):n], col = "purple", lwd = 2, lty = 3)

## Missing Values Within Time Series ##
# What happens if we have missing values within our time series?
y_na_wn <- y
y_na_wn[2:45] <- y_na_wn[234:256] <- NA

model <- list( 
  B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
  Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
  x0=matrix("mu"), tinitx=0 )

fit_kem_y_na_wn <- MARSS(c(y_na_wn), model = model, method = "kem")

kf_y_na_wn <- MARSSkfss(fit_kem_y_na_wn)

# Plot to filter, predictions, and smoother
par(mfrow = c(3, 1))
par(mar = rep(2, 4))
sub <- 1:280

plot(c(y_na_wn)[sub], 
     main = "Kalman Predictions", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf_y_na_wn[["xtt1"]]), col = "red", lwd = 2)
lines(c(kf_y_na_wn[["xtt1"]]) - c(sqrt(kf_y_na_wn[["Vtt1"]])), col = "red", lwd = 2, lty = 3)
lines(c(kf_y_na_wn[["xtt1"]]) + c(sqrt(kf_y_na_wn[["Vtt1"]])), col = "red", lwd = 2, lty = 3)

plot(c(y_na_wn)[sub],
     main = "Kalman Filter", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf_y_na_wn[["xtt"]]), col = "purple", lwd = 2)
lines(c(kf_y_na_wn[["xtt"]]) - c(sqrt(kf_y_na_wn[["Vtt"]])), col = "purple", lwd = 2, lty = 3)
lines(c(kf_y_na_wn[["xtt"]]) + c(sqrt(kf_y_na_wn[["Vtt"]])), col = "purple", lwd = 2, lty = 3)

plot(c(y_na_wn)[sub],
     main = "Kalman Smoother", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(kf_y_na_wn[["xtT"]]), col = "blue", lwd = 2)
lines(c(kf_y_na_wn[["xtT"]]) - c(sqrt(kf_y_na_wn[["VtT"]])), col = "blue", lwd = 2, lty = 3)
lines(c(kf_y_na_wn[["xtT"]]) + c(sqrt(kf_y_na_wn[["VtT"]])), col = "blue", lwd = 2, lty = 3)
