rm(list = ls())

library(MTS) 
library(astsa)

data("soi") # Southern Oscillation Index, related to sea surface temperature
data("rec") # Number of new fish
soi <- soi - mean(soi)
rec <- rec - mean(rec)

X <- cbind(soi, rec)
X <- X[1:400, ]

arMTS <- VAR(X, p = 3, include.mean = FALSE) # Conditional Least Squares
ar <- ar(X, method = "yw", order.max = 3,  
         aic = FALSE)
ar$ar[1, , ]
arMTS$Phi # Same answer!

# We need to use MTS to do VMA, VARMA
maMTS <- VMA(X, q = 1, include.mean = FALSE) # Conditional Likelihood
maMTSe <- VMAe(X, q = 1, include.mean = FALSE) # Exact Likelihood

# We get similar but not identical answers - need to be careful when we are
# fitting movig average models
maMTS$Theta
maMTSe$Theta 

# Now let's do a VARMA model - have to use conditional likelihood
armaMTS <- VARMA(X, p = 1, q = 1, include.mean = FALSE)
armaMTS$Phi
armaMTS$Theta

ps <- 0:1
qs <- 0:1

bics <- matrix(nrow = length(ps), ncol = length(qs))

for (i in 1:length(ps)) {
  for (j in 1:length(qs)) {
    varma <- VARMA(X, p = ps[i], q = qs[j], include.mean = FALSE)
    bics[i, j] <- varma$bic
  }
}

varma.pred <- VARMApred(varma, h = length(soi) - nrow(X))

plot(rec, col = "gray", ylim = c(-200, 200))
lines(time(rec)[1:nrow(X)], X[, 2])
lines(time(rec)[(nrow(X) + 1):length(rec)], varma.pred$pred[, 2], col = "blue")
lines(time(rec)[(nrow(X) + 1):length(rec)], varma.pred$pred[, 2] + qnorm(0.975)*varma.pred$se.err[, 2], col = "blue",
      lty = 2)
lines(time(rec)[(nrow(X) + 1):length(rec)], varma.pred$pred[, 2] - qnorm(0.975)*varma.pred$se.err[, 2], col = "blue",
      lty = 2)

# This software only lets us add covariates to the 
# Example - month indicators
month <- factor(round(time(soi), 3) - floor(time(soi)))

Z <- model.matrix(~month-1)[, -12]
Z.old <- Z[1:nrow(X), ]
Z.new <- Z[(nrow(X) + 1):length(soi), ]

varx <- VARX(X, p = 1, xt = Z.old, include.mean = FALSE, m = 0)
