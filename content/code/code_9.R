rm(list = ls())

library(astsa)
library(MARSS)

## Let's work through an example of modeling seasonality with covariates
data("AirPassengers")
y <- AirPassengers
n <- length(y)

t <- time(y)

# Let's log the data
y <- log(y)
# Let's forecast the last 12 months
y[(n - 12 + 1):n] <- NA


# First we'll fit a super simple model with no seasonality
model.basic <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                    Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                    x0=matrix("mu"), tinitx=0)

fit.basic <- MARSS(c(y), model=model.basic, method = "kem")
fit.basic <- MARSS(c(y), model=model.basic,  method = "BFGS",
                   inits = fit.basic)

# Let's look at the forecasts from
plot(log(AirPassengers), ylim = range(c(y, c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)), na.rm = TRUE),
     col = "gray", type = "l")
lines(t[!is.na(y)], y[!is.na(y)])
lines(t[is.na(y)], c(fit.basic$ytT)[is.na(y)], col = "red")
lines(t[is.na(y)], c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)[is.na(y)], col = "red",
      lty = 2)
lines(t[is.na(y)], c(fit.basic$ytT - qnorm(0.975)*fit.basic$ytT.se)[is.na(y)], col = "red",
      lty = 2)
# These forecasts aren't very good!
# Let's fit a model with indicators for the month.
# First we have to make a month variable
month <- factor(round((round(time(y), 3) - floor(time(y)))*12))
# Make a design matrix of indicators for each month. We don't need
# the last one because we have an overall scale variable
covariates <- t(model.matrix(~month-1)[, -12])

# We'll put the covariates in the observation part of the equation
model.covy <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   D="unconstrained", d=covariates,
                   x0=matrix("mu"), tinitx=0 )

fit.covy <- MARSS(c(y), model=model.covy, method = "kem")
fit.covy <- MARSS(c(y), model=model.covy,  method = "BFGS",
                  inits = fit.covy)

# Do we get better forecasts?
lines(t[is.na(y)], c(fit.covy$ytT)[is.na(y)], col = "blue")
lines(t[is.na(y)], c(fit.covy$ytT + qnorm(0.975)*fit.covy$ytT.se)[is.na(y)], col = "blue",
      lty = 2)
lines(t[is.na(y)], c(fit.covy$ytT - qnorm(0.975)*fit.covy$ytT.se)[is.na(y)], col = "blue",
      lty = 2)
# Yes! These are *much* better

# Now let's fit a stochastic regression model
# We'll try fitting stochastic regression model
# where we allow for a conditionally linear time trend,
# with the regression coefficient evolving over time
data(gnp)
y <- c(gnp)
t <- time(gnp)
n <- length(y)

# For this we'll forecast the last 24 quarters
y[(n - 24 + 1):n] <- NA

# We want to let the 
# Since a_t isn't going to be estimated from the data anymore as a constant, we'll let 
# the state equation have an overall level u
model.streg <- list(B=matrix("phi"), U=matrix("u"), Q=matrix("sig.sq.w"), 
                    Z=array(1:n, dim = c(1, 1, n)), A=matrix(0), R=matrix("sig.sq.v"),
                    x0=matrix("mu"), tinitx=0 )

fit.streg <- MARSS(c(y), model=model.streg, method = "kem")
fit.streg <- MARSS(c(y), model=model.streg,  method = "BFGS",
                   inits = fit.streg)

# Let's get the one-step ahead forecasts of the states, which here are our 
# time-varying regression coefficients
kf.streg <- MARSSkfss(fit.streg)

plot(t, gnp, main = "Quarterly US GNP", ylab = expression(x[t]), xlab = "Time", type = "l",
     ylim = range(c(y, gnp), na.rm = TRUE), col = "gray")
lines(t[!is.na(y)], y[!is.na(y)])
lines(c(t), c((1:n)*kf.streg$xtt1), col = "blue")
ses <- sqrt((1:n)^2*kf.streg$Vtt1 + coef(fit.streg)$R[1, 1])
lines(c(t), c((1:n)*kf.streg$xtt1) + qnorm(0.975)*ses, col = "blue", lty = 2)
lines(c(t), c((1:n)*kf.streg$xtt1) - qnorm(0.975)*ses, col = "blue", lty = 2)
# Interesting! We get a nearly but not exactly linear fit. 
# Some students asked about the weird behavior at the beginning of the time series.
# This is probably a result of treating the initial regression coefficient
# value as fixed. Still, some students wondered if it could actually be
# because we specified an initial value of x_0 instead of x_1. Let's try 
# setting the initial value at x_1 - thanks to a student for pointing out how
# to do this by resetting tinitx!

model.streg1 <- list(B=matrix("phi"), U=matrix("u"), Q=matrix("sig.sq.w"), 
                    Z=array(1:n, dim = c(1, 1, n)), A=matrix(0), R=matrix("sig.sq.v"),
                    x0=matrix("mu"), tinitx=1 )

fit.streg1 <- MARSS(c(y), model=model.streg1, method = "BFGS", inits = fit.streg)

# Let's get the one-step ahead forecasts of the states, which here are our 
# time-varying regression coefficients
kf.streg1 <- MARSSkfss(fit.streg1)

lines(c(t), c((1:n)*kf.streg1$xtt1), col = "red")
ses <- sqrt((1:n)^2*kf.streg1$Vtt1 + coef(fit.streg1)$R[1, 1])
lines(c(t), c((1:n)*kf.streg1$xtt1) + qnorm(0.975)*ses, col = "red", lty = 2)
lines(c(t), c((1:n)*kf.streg1$xtt1) - qnorm(0.975)*ses, col = "red", lty = 2)
# Changing where we initialize doesn't matter much. 

# What if instead of fixing the initial value, we assumed it had a distribution with a very large variance
# V0 controls the variance
model.streg.varx1 <- list(B=matrix("phi"), U=matrix("u"), Q=matrix("sig.sq.w"), 
                     Z=array(1:n, dim = c(1, 1, n)), A=matrix(0), R=matrix("sig.sq.v"),
                     x0=matrix("mu"), V0=matrix(10000), tinitx=0)

fit.streg.varx1 <- MARSS(c(y), model=model.streg.varx1, method = "BFGS", inits = fit.streg)

# Let's get the one-step ahead forecasts of the states, which here are our 
# time-varying regression coefficients
kf.streg1.varx1 <- MARSSkfss(fit.streg.varx1)

plot(t, gnp, main = "Quarterly US GNP", ylab = expression(x[t]), xlab = "Time", type = "l",
     ylim = range(c(y, gnp), na.rm = TRUE), col = "gray")
lines(t[!is.na(y)], y[!is.na(y)])
lines(c(t), c((1:n)*kf.streg$xtt1), col = "blue")
ses <- sqrt((1:n)^2*kf.streg$Vtt1 + coef(fit.streg)$R[1, 1])
lines(c(t), c((1:n)*kf.streg$xtt1) + qnorm(0.975)*ses, col = "blue", lty = 2)
lines(c(t), c((1:n)*kf.streg$xtt1) - qnorm(0.975)*ses, col = "blue", lty = 2)

lines(c(t), c((1:n)*kf.streg1.varx1$xtt1), col = "purple")
ses <- sqrt((1:n)^2*kf.streg1.varx1$Vtt1 + coef(fit.streg.varx1)$R[1, 1])
lines(c(t), c((1:n)*kf.streg1.varx1$xtt1) + qnorm(0.975)*ses, col = "purple", lty = 2)
lines(c(t), c((1:n)*kf.streg1.varx1$xtt1) - qnorm(0.975)*ses, col = "purple", lty = 2)
# This does change things a bit, but we still have some trouble at the initial value.
# Fortunately we're usually just interested in forecasting, so as long
# as our time series is long enough it's not a big deal to have some trouble modeling the start
# of the time series

