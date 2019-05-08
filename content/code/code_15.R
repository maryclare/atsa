# rm(list = ls())

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
model0 <-  list(
  ### Outcome Equation
  Z=matrix(1,2,1), # These are our A values from our observation equation 
                   # - We have two outcomes and one state so it is a 2\times 1 vector
                   # - We're going to start by setting A to the one vector
  A=matrix(0, 2, 1), # This is a matrix of intercepts for each observation, 
                     # we generally set it to 0 since overall level is captured in the mean of the initial value and a
  R="diagonal and unequal", # This is the variance-covariance matrix of the observation equation
                            # Stating "diagonal and unequal" sets the correlation terms to zero
  ### Outcome Equation
  B=matrix(1), # This is our single phi value (since we just have a single state variable), we're going to set it to 1
  U=matrix(0, 1, 1), # This is a matrix of intercepts for each state, 
                     # again we generally set it to 0 since overall level is captured in the mean of the initial value
  Q=matrix("q"), # This is the variance of the state equation (just a scalar because we have one state)
  ### Initial value(s)
  x0=matrix("mu"), 
  tinitx=1) # This last argument tells us that mu will be x_1

fit0 <- MARSS(t(Y), model=model0, method = "kem")
fit0 <- MARSS(t(Y), model=model0, method = "BFGS", inits = fit0)

par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit0$ytT[2, is.na(Y[, 2])])
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)

plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit0$ytT[1, is.na(Y[, 1])])
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)

# These are very bad! Let's fit the same model again, but get rid of the restrictions
model1 <-  list(
  ### Outcome Equation
  Z=matrix(c("a11", "a12"),2,1), # These are our A values from our observation equation 
  # - We have two outcomes and one state so it is a 2\times 1 vector
  # - We're going to let the values of A be bestimated now
  A=matrix(0, 2, 1), # This is a matrix of intercepts for each observation, 
  # we generally set it to 0 since overall level is captured in the mean of the initial value and a
  R="unconstrained", # This is the variance-covariance matrix of the observation equation
                     # Stating "unconstrained" lets us estimate the correlation
  ### State Equation
  B=matrix("phi"), # This is our single phi value (since we just have a single state variable), 
                   # We're going to let it take on different values now
  U=matrix(0, 1, 1), # This is a matrix of intercepts for each state, 
  # again we generally set it to 0 since overall level is captured in the mean of the initial value
  Q=matrix("q"), # This is the variance of the state equation (just a scalar because we have one state)
  ### Initial value(s)
  x0=matrix("mu"), 
  tinitx=1) # This last argument tells us that mu will be x_1

fit1 <- MARSS(t(Y), model=model1, method = "kem")
fit1 <- MARSS(t(Y), model=model1, method = "BFGS", inits = fit1)

# Add to our plot!
par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit0$ytT[2, is.na(Y[, 2])])
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)


lines(time.new, fit1$ytT[2, is.na(Y[, 2])], col = "blue")
lines(time.new, fit1$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit1$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "blue")
lines(time.new, fit1$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit1$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "blue")


plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit0$ytT[1, is.na(Y[, 1])])
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)

lines(time.new, fit1$ytT[1, is.na(Y[, 1])], col = "blue")
lines(time.new, fit1$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit1$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")
lines(time.new, fit1$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit1$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")
# Narrower bands, but still pretty implausible looking

# Let's add one more state
model2 <-  list(
  ### Outcome Equation
  Z=matrix(c("a11", "a21", "a12", "a22"),2,2), # These are our A values from our observation equation 
  # - We have two outcomes and two states now so it is a 2\times 2 vector
  # - We're going to let the values of A be bestimated now
  A=matrix(0, 2, 1), # This is a matrix of intercepts for each observation, 
  # we generally set it to 0 since overall level is captured in the mean of the initial value and a
  R="unconstrained", # This is the variance-covariance matrix of the observation equation
                     # Stating "unconstrained" lets us estimate the correlation
  ### State Equation
  B=matrix(c("phi11", "phi21", "phi12", "phi22"),2,2), # This is our single phi value (since we just have a single state variable), 
  # We're going to let it take on different values now
  U=matrix(0, 2, 1), # This is a matrix of intercepts for each state, 
  # again we generally set it to 0 since overall level is captured in the mean of the initial value
  Q="unconstrained", # This is the variance-covariance matrix of the state equation (just a scalar because we have one state)
  ### Initial value(s)
  x0=matrix("mu", 2, 1), 
  tinitx=1) # This last argument tells us that mu will be x_1

fit2 <- MARSS(t(Y), model=model2, method = "kem")
fit2 <- MARSS(t(Y), model=model2, method = "BFGS", inits = fit2)

# Add to our plot!
par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit0$ytT[2, is.na(Y[, 2])])
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)


lines(time.new, fit1$ytT[2, is.na(Y[, 2])], col = "blue")
lines(time.new, fit1$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit1$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "blue")
lines(time.new, fit1$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit1$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "blue")

lines(time.new, fit2$ytT[2, is.na(Y[, 2])], col = "red")
lines(time.new, fit2$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit2$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "red")
lines(time.new, fit2$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit2$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "red")

plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit0$ytT[1, is.na(Y[, 1])])
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)

lines(time.new, fit1$ytT[1, is.na(Y[, 1])], col = "blue")
lines(time.new, fit1$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit1$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")
lines(time.new, fit1$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit1$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")

lines(time.new, fit2$ytT[1, is.na(Y[, 1])], col = "red")
lines(time.new, fit2$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit2$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "red")
lines(time.new, fit2$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit2$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "red")


# Let's add even one more states
model3 <-  list(
  ### Outcome Equation
  Z=matrix(c("a11", "a12", "a13", "a21", "a22", "a23"),2,3, byrow = TRUE), # These are our A values from our observation equation 
  # - We have two outcomes and three states now so it is a 2\times 3 vector
  A=matrix(0, 2, 1), # This is a matrix of intercepts for each observation, 
  # we generally set it to 0 since overall level is captured in the mean of the initial value and a
  R="unconstrained", # This is the variance-covariance matrix of the observation equation
  # Stating "unconstrained" lets us estimate the correlation
  ### State Equation
  B=matrix(c("phi11", "phi12", "phi13", "phi21", "phi22", "phi23", "phi31", "phi32", "phi33"),3,3, byrow=TRUE), # This is our 3x3 phi matrix 
  U=matrix(0, 3, 1), # This is a matrix of intercepts for each state, 
  # again we generally set it to 0 since overall level is captured in the mean of the initial value
  Q="unconstrained", # This is the variance-covariance matrix of the state equation (just a scalar because we have one state)
  ### Initial value(s)
  x0=matrix("mu", 3, 1), 
  tinitx=1) # This last argument tells us that mu will be x_1

fit3 <- MARSS(t(Y), model=model3, method = "kem")
fit3 <- MARSS(t(Y), model=model3, method = "BFGS", inits = fit3)

# Add to our plot!
par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit0$ytT[2, is.na(Y[, 2])])
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit0$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit0$ytT.se[2, is.na(Y[, 2])], lty = 2)


lines(time.new, fit1$ytT[2, is.na(Y[, 2])], col = "blue")
lines(time.new, fit1$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit1$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "blue")
lines(time.new, fit1$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit1$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "blue")

lines(time.new, fit2$ytT[2, is.na(Y[, 2])], col = "red")
lines(time.new, fit2$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit2$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "red")
lines(time.new, fit2$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit2$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "red")


lines(time.new, fit3$ytT[2, is.na(Y[, 2])], col = "purple")
lines(time.new, fit3$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit3$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "purple")
lines(time.new, fit3$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit3$ytT.se[2, is.na(Y[, 2])], lty = 2, col = "purple")


plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit0$ytT[1, is.na(Y[, 1])])
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit0$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit0$ytT.se[1, is.na(Y[, 1])], lty = 2)

lines(time.new, fit1$ytT[1, is.na(Y[, 1])], col = "blue")
lines(time.new, fit1$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit1$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")
lines(time.new, fit1$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit1$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")

lines(time.new, fit2$ytT[1, is.na(Y[, 1])], col = "red")
lines(time.new, fit2$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit2$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "red")
lines(time.new, fit2$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit2$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "red")

lines(time.new, fit3$ytT[1, is.na(Y[, 1])], col = "purple")
lines(time.new, fit3$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit3$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "purple")
lines(time.new, fit3$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit3$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "purple")


# Adding two states doesn't change things much
AIC(fit0)
AIC(fit1)
AIC(fit2)
AIC(fit3)

BIC(fit0)
BIC(fit1)
BIC(fit2)
BIC(fit3)

# Now let's try a MUCH more complicated model with monthly affects, 
# but go back to two states
Z <- model.matrix(~factor(round(time(soi), 3) - floor(time(soi))))
Z <- Z[, -ncol(Z)]

model.s <-  list(
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
fit.s <- MARSS(t(Y), model=model.s, method = "kem")
fit.s <- MARSS(t(Y), model=model.s, method = "BFGS", inits = fit.s)

par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit.s$ytT[2, is.na(Y[, 2])])
lines(time.new, fit.s$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit.s$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit.s$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit.s$ytT.se[2, is.na(Y[, 2])], lty = 2)


plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit.s$ytT[1, is.na(Y[, 1])])
lines(time.new, fit.s$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit.s$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit.s$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit.s$ytT.se[1, is.na(Y[, 1])], lty = 2)

AIC(fit0)
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit.s)

BIC(fit0)
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit.s)

Z <- model.matrix(~factor(round(time(soi), 3) - floor(time(soi))))
Z <- Z[, -ncol(Z)]

model4 <-  list(
  ### Outcome Equation
  Z=matrix(c("a11", "a12", "a13", "a21", "a22", "a23"),2,3, byrow = TRUE), # These are our A values from our observation equation 
  # - We have two outcomes and three states now so it is a 2\times 3 vector
  A=matrix(0, 2, 1), # This is a matrix of intercepts for each observation, 
  # we generally set it to 0 since overall level is captured in the mean of the initial value and a
  R="unconstrained", # This is the variance-covariance matrix of the observation equation
  # Stating "unconstrained" lets us estimate the correlation
  ### State Equation
  B=matrix(c("phi11", "phi12", "phi13", "phi21", "phi22", "phi23", "phi31", "phi32", "phi33"),3,3, byrow=TRUE), # This is our 3x3 phi matrix 
  U=matrix(0, 3, 1), # This is a matrix of intercepts for each state, 
  # again we generally set it to 0 since overall level is captured in the mean of the initial value
  Q="unconstrained", # This is the variance-covariance matrix of the state equation (just a scalar because we have one state)
  C = matrix(paste("c", rep(1:11, each = 3), rep(1:3, times = 11), sep = ""), 3, 11),
  c = t(Z),
  ### Initial value(s)
  x0=matrix("mu", 3, 1), 
  tinitx=1) # This last argument tells us that mu will be x_1

fit4 <- MARSS(t(Y), model=model4, method = "kem")
fit4 <- MARSS(t(Y), model=model4, method = "BFGS", inits = fit4)


par(mfrow = c(1, 2))
plot(c(rec - mean(rec)), col = "gray", type = "l", ylim = c(-60, 60), xlim = c(250, length(rec)),
     main = "SOI")
time.new <- sum(!is.na(Y[, 2])) + 1:(length(rec) - sum(!is.na(Y[, 2])))
lines(time.new, fit.s$ytT[2, is.na(Y[, 2])])
lines(time.new, fit.s$ytT[2, is.na(Y[, 2])] + qnorm(0.975)*fit.s$ytT.se[2, is.na(Y[, 2])], lty = 2)
lines(time.new, fit.s$ytT[2, is.na(Y[, 2])] + qnorm(0.025)*fit.s$ytT.se[2, is.na(Y[, 2])], lty = 2)


plot(c(soi - mean(soi)), col = "gray", type = "l", xlim = c(250, length(soi)), main = "Recruitment")
time.new <- sum(!is.na(Y[, 1])) + 1:(length(rec) - sum(!is.na(Y[, 1])))
lines(time.new, fit4$ytT[1, is.na(Y[, 1])])
lines(time.new, fit4$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit4$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit4$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit4$ytT.se[1, is.na(Y[, 1])], lty = 2)
lines(time.new, fit.s$ytT[1, is.na(Y[, 1])], col = "blue")
lines(time.new, fit.s$ytT[1, is.na(Y[, 1])] + qnorm(0.975)*fit.s$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")
lines(time.new, fit.s$ytT[1, is.na(Y[, 1])] + qnorm(0.025)*fit.s$ytT.se[1, is.na(Y[, 1])], lty = 2, col = "blue")

BIC(fit.s)
BIC(fit4)
