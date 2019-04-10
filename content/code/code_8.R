rm(list = ls())

library(astsa)
library(MARSS)

data("varve")
n <- length(varve)

y <- log(varve)
plot(y)

model <- list(
  B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
  Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
  x0=matrix("mu"), tinitx=0 )

fit <- MARSS(c(y), model=model, method = "kem")
# Get Kalman filter, predictor and smoother
kf <- MARSSkfss(fit)


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


plot(c(y)[sub], main = "Kalman Smoother", xlab = "Time", ylab = "y", col = "darkgray", type = "b", pch = 16, lty = 1)
lines(c(fit$states), col = "blue", lwd = 2)
lines(c(fit$states) - c(fit$states.se), col = "blue", lwd = 2, lty = 3)
lines(c(fit$states) + c(fit$states.se), col = "blue", lwd = 2, lty = 3)

par(mfrow = c(1, 1))
plot(c(y), type = "l", col = "darkgray", main = "Kalman Smoother", xlab = "Time", ylab = "y")
lines(c(fit$states), col = "blue", lwd = 1)
lines(c(fit$states) - c(fit$states.se), col = "blue", lwd = 1, lty = 3)
lines(c(fit$states) + c(fit$states.se), col = "blue", lwd = 1, lty = 3)

