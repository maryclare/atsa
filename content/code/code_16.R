rm(list = ls())

library(MTS) 
library(astsa)

data("soi") # Southern Oscillation Index, related to sea surface temperature
data("rec") # Number of new fish
soi <- soi - mean(soi)
rec <- rec - mean(rec)

X <- cbind(soi, rec)
X <- X[1:400, ]

# This software only lets us add covariates to the VAR model
Z <- 1:nrow(X)

varx <- VARX(X, p = 1, xt = Z, include.mean = FALSE, m = 0)
# Hard to get predictions out...

linfit <- lm(X~Z)

armaMTS <- VARMA(linfit$residuals, p = 1, q = 1, include.mean = FALSE)
armaMTS$Phi
armaMTS$Theta

varma.pred <- VARMApred(armaMTS, h = length(soi) - nrow(X))

plot(rec, col = "gray", ylim = c(-200, 200))
lines(time(rec)[1:nrow(X)], X[, 2])
# We'll need to add the fitted values onto our forecasts
lines(time(rec)[(nrow(X) + 1):length(rec)], varma.pred$pred[, 2] + linfit$coefficients[, 2][1] + linfit$coefficients[, 2][2]*(nrow(X) + 1:nrow(varma.pred$pred)), col = "blue")
lines(time(rec)[(nrow(X) + 1):length(rec)], varma.pred$pred[, 2] + linfit$coefficients[, 2][1] + linfit$coefficients[, 2][2]*(nrow(X) + 1:nrow(varma.pred$pred)) + qnorm(0.975)*varma.pred$se.err[, 2], col = "blue",
      lty = 2)
lines(time(rec)[(nrow(X) + 1):length(rec)], varma.pred$pred[, 2]+ linfit$coefficients[, 2][1] + linfit$coefficients[, 2][2]*(nrow(X) + 1:nrow(varma.pred$pred)) - qnorm(0.975)*varma.pred$se.err[, 2], col = "blue",
      lty = 2)

