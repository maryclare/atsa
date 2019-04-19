# Authosr: Megan Gelsinger (annotations), Maryclare Griffin (code) 
# Date: 4/19/19
# Title: STSCI 4550 Lab Session 10 
###################################

# In this lab, we are going to review how to include covariates (seasonal effect) into 
# state-space models.  

rm(list = ls())

library(astsa)
library(MARSS)

data("AirPassengers")
y <- AirPassengers
n <- length(y)

t <- time(y)
t # We have monthly data

# Let's forecast the last 12 months
y <- log(y)
y[(n - 12 + 1):n] <- NA

# In our first model, we won't account for any seasonality. 
model.basic <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   x0=matrix("mu"), tinitx=0)

fit.basic <- MARSS(c(y), model=model.basic, method = "kem")
fit.basic <- MARSS(c(y), model=model.basic,  method = "BFGS",
                  inits = fit.basic)

kf.basic <- MARSSkfss(fit.basic)

# Was this model a good fit?  Did it make accurate forecasts?
plot(log(AirPassengers), ylim = range(c(y, c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)), na.rm = TRUE),
     col = "gray", type = "l")
lines(t[!is.na(y)], y[!is.na(y)])
lines(as.numeric(t) , coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1), col = "blue")
ses <- sqrt(coef(fit.basic)$Z[1, 1]^2*c(kf.basic$Vtt1) + coef(fit.basic)$R[1, 1])
lines(as.numeric(t), coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1) + qnorm(0.975)*ses, col = "blue", lty = 2)
lines(as.numeric(t), coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1) - qnorm(0.975)*ses, col = "blue", lty = 2)

# Let's look at another visual.
plot(log(AirPassengers), ylim = range(c(y, c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)), na.rm = TRUE),
     col = "gray", type = "l")
lines(t[!is.na(y)], y[!is.na(y)])
lines(t[is.na(y)], c(fit.basic$ytT)[is.na(y)], col = "red") # Wait, this looks like different code that is getting us the same values...
# Let's compare this to the way we got the forecasted values before:
lines(as.numeric(t)[is.na(y)] , coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1)[is.na(y)], col = "blue")
# Hey, they ARE the same! Why is this?
# Okay, moving on...
lines(t[is.na(y)], c(fit.basic$ytT)[is.na(y)], col = "red")
#lines(as.numeric(t)[is.na(y)] , coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1)[is.na(y)], col = "blue")
lines(t[is.na(y)], c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)[is.na(y)], col = "red",
      lty = 2)
# lines(as.numeric(t)[is.na(y)], (coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1) + qnorm(0.975)*ses)[is.na(y)], col = "blue", lty = 2)
lines(t[is.na(y)], c(fit.basic$ytT - qnorm(0.975)*fit.basic$ytT.se)[is.na(y)], col = "red",
      lty = 2)
# lines(as.numeric(t)[is.na(y)], (coef(fit.basic)$Z[1, 1]*c(kf.basic$xtt1) + qnorm(0.975)*ses)[is.na(y)], col = "blue", lty = 2)

# Let's try and add a seasonal effect to improve our model fit and forecasts.  
# As we discussed in class, we can add the seasonal effect in the observation equation,
# the state equation, or both.  
# Our next model will have a seasonal effect in the observation equation. 
month <- factor(round((round(time(y), 3) - floor(time(y)))*12))
covariates <- t(model.matrix(~month-1)[, -12]) # -12: need reference level

model.covy <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
              Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
              D="unconstrained", d=covariates,
              x0=matrix("mu"), tinitx=0 )

fit.covy <- MARSS(c(y), model=model.covy, method = "kem")
# Why didn't we also do the "BFGS" fit?

kf.covy <- MARSSkfss(fit.covy)

# Was this model a good fit?  Did it make accurate forecasts?
plot(log(AirPassengers), ylim = range(c(y, c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)), na.rm = TRUE),
     col = "gray", type = "l")
lines(t[!is.na(y)], y[!is.na(y)])
lines(t[is.na(y)], c(fit.covy$ytT)[is.na(y)], col = "blue")
lines(t[is.na(y)], c(fit.covy$ytT + qnorm(0.975)*fit.covy$ytT.se)[is.na(y)], col = "blue",
      lty = 2)
lines(t[is.na(y)], c(fit.covy$ytT - qnorm(0.975)*fit.covy$ytT.se)[is.na(y)], col = "blue",
      lty = 2)

# Our next model will have a seasonal effect in the state equation. 
model.covx <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
              C="unconstrained", c=covariates,
              Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
              x0=matrix("mu"), tinitx=0 )

fit.covx <- MARSS(c(y), model=model.covx, method = "kem")

# Why don't we need to apply the Kalman filter?

# Model 1 (no seasonality)
plot(log(AirPassengers), xlim = c(max(t) - 3, max(t)), ylim = range(c(y, c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)), na.rm = TRUE),
     col = "gray", type = "l")
lines(t[!is.na(y)], y[!is.na(y)])
lines(t[is.na(y)], c(fit.basic$ytT)[is.na(y)], col = "red")
lines(t[is.na(y)], c(fit.basic$ytT + qnorm(0.975)*fit.basic$ytT.se)[is.na(y)], col = "red",
      lty = 2)
lines(t[is.na(y)], c(fit.basic$ytT - qnorm(0.975)*fit.basic$ytT.se)[is.na(y)], col = "red",
      lty = 2)
# Model 2 (seasonal effect in observation equation)
lines(t[is.na(y)], c(fit.covy$ytT)[is.na(y)], col = "blue", lwd = 2)
lines(t[is.na(y)], c(fit.covy$ytT + qnorm(0.975)*fit.covy$ytT.se)[is.na(y)], col = "blue",
      lty = 2, lwd = 1.5)
lines(t[is.na(y)], c(fit.covy$ytT - qnorm(0.975)*fit.covy$ytT.se)[is.na(y)], col = "blue",
      lty = 2, lwd = 1.5)
# Model 3 (seasonal effect in state equation)
lines(t[is.na(y)], c(fit.covx$ytT)[is.na(y)], col = "purple", lwd = 2)
lines(t[is.na(y)], c(fit.covx$ytT + qnorm(0.975)*fit.covx$ytT.se)[is.na(y)], col = "purple",
      lty = 2, lwd = 1.5)
lines(t[is.na(y)], c(fit.covx$ytT - qnorm(0.975)*fit.covx$ytT.se)[is.na(y)], col = "purple",
      lty = 2, lwd = 1.5)

# Our final model will have a seasonal effect in the both the observation equation and 
# the state equation.
model.covxy <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                   C="unconstrained", c=covariates,
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   D="unconstrained", d=covariates,
                   x0=matrix("mu"), tinitx=0 )

fit.covxy <- MARSS(c(y), model=model.covxy, method = "kem")

lines(t[is.na(y)], c(fit.covxy$ytT)[is.na(y)], col = "green", lwd = 2)
lines(t[is.na(y)], c(fit.covxy$ytT + qnorm(0.975)*fit.covxy$ytT.se)[is.na(y)], col = "green",
      lty = 2, lwd = 1.5)
lines(t[is.na(y)], c(fit.covxy$ytT - qnorm(0.975)*fit.covxy$ytT.se)[is.na(y)], col = "green",
      lty = 2, lwd = 1.5 )

# Let's compare all of these models with AIC to pick the best one
AIC(fit.basic) 
AIC(fit.covy)
AIC(fit.covx)
AIC(fit.covxy)

