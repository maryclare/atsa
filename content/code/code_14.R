rm(list = ls())

library(astsa)
library(MARSS) 

data("soi") # Southern Oscillation Index, related to sea surface temperature
data("rec") # Number of new fish
soi <- soi - mean(soi)
rec <- rec - mean(rec)

Y <- cbind(c(soi), c(rec))
Y[401:nrow(Y), ] <- NA

# Simplest example first - probably won't give good forecasts but worth trying!
# It only lets us have a single latent time series, and we assume the elements of phi
# and a are all 1. We also assume that the errors in the observation equation are uncorrelated.
model <-  list(
  B=matrix(1),
  U=matrix(0, 1, 1),
  Q=matrix("q"),
  Z=matrix(1,2,1), # Observation equation
  A=matrix(0, 2, 1),
  R="diagonal and unequal",
  x0=matrix("mu"),
  tinitx=1)

fit <- MARSS(t(Y), model=model, method = "kem")
fit <- MARSS(t(Y), model=model, method = "BFGS", inits = fit)

par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit$ytT[2, is.na(Y[, 2])])
lines(time.new, fit$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit$ytT.se[2, is.na(Y[, 2])], lty = 2)

plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit$ytT[1, is.na(Y[, 1])])
lines(time.new, fit$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit$ytT.se[1, is.na(Y[, 1])], lty = 2)

# Now let's try a MUCH more complicated model
Z <- model.matrix(~factor(round(time(soi), 3) - floor(time(soi))))
Z <- Z[, -ncol(Z)]

model <-  list(
  B=matrix(c("phi11", "phi21", "phi12", "phi22"), nrow = 2, ncol = 2),
  U=matrix(0, 2, 1),
  Q="unconstrained",
  Z=matrix(c("a11", "a21", "a12", "a22"),2, 2), # Observation equation
  A=matrix(0, 2, 1),
  R="unconstrained",
  D = matrix(paste("d", rep(1:11, each = 2), rep(1:2, times = 11), sep = ""), 2, 11),
  d = t(Z),
  x0=matrix("mu", 2, 1),
  tinitx=1)

# This takes a long time!! You have been warned.
fit <- MARSS(t(Y), model=model, method = "kem")
fit <- MARSS(t(Y), model=model, method = "BFGS", inits = fit)

par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit$ytT[2, is.na(Y[, 2])])
lines(time.new, fit$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit$ytT.se[2, is.na(Y[, 2])], lty = 2)

plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit$ytT[1, is.na(Y[, 1])])
lines(time.new, fit$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit$ytT.se[1, is.na(Y[, 1])], lty = 2)

