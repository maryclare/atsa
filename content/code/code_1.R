rm(list = ls())

# Define the autocovariance function for a mean-zero
# ARMA(1, 1) model
gamma.x <- function(h, phi1, theta1, sig.sq.w) {
  h <- abs(h) 
  if (h == 0) {
    g.x <- (theta1^2 + 2*phi1*theta1 + 1)*sig.sq.w/(1 - phi1^2)
  } else {
    g.x <- sig.sq.w*phi1^(h - 1)*((1 + theta1*phi1)*(phi1 + theta1)/(1 - phi1^2))
  }
  return(g.x)
}

# Function to directly solve the forecasting equation by inverting A_n
# Also returns v_n
solve.direct <- function(n, phi1 = 0, theta1 = 0, sig.sq.w = 1) {
  A.n <- matrix(nrow = n, ncol = n)
  b.n <- numeric(n)
  for (i in 1:n) {
    b.n[i] <- gamma.x(i, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w)
    for (j in 1:n) {
      A.n[i, j] <- gamma.x(h = i - j, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w)
    }
  }
  c.n <- solve(A.n)%*%b.n
  v.n <- gamma.x(0, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w) + t(c.n)%*%A.n%*%c.n - 2*t(c.n)%*%b.n
  return(list("c.n" = c.n, "v.n" = v.n))
}

# Durbin-Levinson algorithm, recovers all of the one-step-ahead prediction coefficients 
# up to one future prediction and returns the expected squared error loss of each 
# one-step-ahead prediction
solve.dl <- function(n, phi1 = 0, theta1 = 0, sig.sq.w = 1) {
  C <- matrix(nrow = n, ncol = n)
  v <- rep(NA, n + 1)
  gamma.x.0 <- gamma.x(0, 
                       phi1 = phi1, 
                       theta1 = theta1, 
                       sig.sq.w = sig.sq.w)
  C[1, 1] <- gamma.x(1, 
                     phi1 = phi1, 
                     theta1 = theta1, 
                     sig.sq.w = sig.sq.w)/gamma.x.0
  v[1] <- gamma.x.0
  v[2] <- v[1]*(1 - C[1, 1]^2)
  for (i in 2:n) {
    C[i, i] <- gamma.x(i, 
                       phi1 = phi1, 
                       theta1 = theta1, 
                       sig.sq.w = sig.sq.w)
    for (j in 1:(i - 1)) {
      C[i, i] <- C[i, i] - C[i-1, j]*gamma.x(i - j, 
                                             phi1 = phi1, 
                                             theta1 = theta1, 
                                             sig.sq.w = sig.sq.w)
    }
    C[i, i] <- C[i, i]/v[i]
    for (j in (i-1):1) {
      C[i, j] <- C[i - 1, j] - C[i, i]*C[i - 1, i - j]
    }
    v[i + 1] <- v[i]*(1 - C[i, i]^2)
  }
  return(list("C"=C, "c.n" = C[nrow(C), ], 
              "v" = v, "v.n" = v[length(v)]))
}

# Let's look at the coefficients and expected squared error losses!

# First, just consider AR(1)
phi1 <- 0.5
theta1 <- 0
sig.sq.w <- 1
n <- 5
dl <- solve.dl(n = n, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w)
dl
# What's happening to our expected prediction loss as we get more data?
plot(dl$v, type = "b")
abline(h = 0, lty = 3)

# What do the partial autocorrelation coefficients look like?
plot(diag(dl$C), type = "b", ylab = expression(c[jj]), xlab = "j")
abline(h = 0, lty = 3)

# Now let's look at an MA(1) model
phi1 <- 0
theta1 <- 0.5
sig.sq.w <- 1
n <- 5
dl <- solve.dl(n = n, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w)
dl
# What's happening to our expected prediction loss as we get more data?
plot(dl$v, type = "b")
abline(h = 0, lty = 3)

# What do the partial autocorrelation coefficients look like?
plot(diag(dl$C), type = "b", ylab = expression(c[jj]), xlab = "j")
abline(h = 0, lty = 3)

# Put the pices together, look at an ARMA(1, 1) model
phi1 <- 0.5
theta1 <- -0.25
sig.sq.w <- 1
n <- 5
dl <- solve.dl(n = n, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w)
dl
# What's happening to our expected prediction loss as we get more data?
plot(dl$v, type = "b")
abline(h = 0, lty = 3)

# What do the partial autocorrelation coefficients look like?
plot(diag(dl$C), type = "b", ylab = expression(c[jj]), xlab = "j")
abline(h = 0, lty = 3)

# Now let's type up the innovation algorithm
solve.innov <- function(n, phi1 = 0, theta1 = 0, sig.sq.w = 1) {
  D <- matrix(nrow = n, ncol = n)
  v <- rep(NA, n + 1)
  gamma.x.0 <- gamma.x(0, 
                       phi1 = phi1, 
                       theta1 = theta1, 
                       sig.sq.w = sig.sq.w)
  v[1] <- gamma.x.0
  for (i in 1:n) {
    for (j in 0:(i-1)) {
      D[i, i - j] <- gamma.x(i - j, 
                             phi1 = phi1, 
                             theta1 = theta1, 
                             sig.sq.w = sig.sq.w)
      if (j > 0) {
        for (k in 0:(j - 1)) {
          D[i, i - j] <- D[i, i - j] - D[j, j - k]*D[i, i - k]*v[k + 1]
        }
      }
      D[i, i - j] <- D[i, i - j]/v[j + 1]
    }
    v[i + 1] <- gamma.x.0 - sum(D[i, i:1]^2*v[1:i], na.rm = TRUE)
  }
  return(list("D"=D, "d.n" = D[nrow(D), ], 
              "v" = v, "v.n" = v[length(v)]))
}

# What do the innovation coefficients look like? 
# Again, we'll go one at a time, first looking at an AR(1) model,
# then an MA(1) model, and then an ARMA(1, 1) model

# AR(1)
phi1 <- 0.5
theta1 <- 0
sig.sq.w <- 1
n <- 5
plot(solve.innov(n, phi1 = phi1, theta1 = theta1)$d.n, type = "b", ylab = expression(d["5j"]), xlab = "j")

# MA(1)
phi1 <- 0
theta1 <- 0.5
sig.sq.w <- 1
n <- 5
plot(solve.innov(n, phi1 = phi1, theta1 = theta1)$d.n, type = "b", ylab = expression(d["5j"]), xlab = "j")

# Go back to AR(1) - do the theta's look familiar??
phi1 <- 0.5
theta1 <- 0
sig.sq.w <- 1
n <- 5
plot(solve.innov(n, phi1 = phi1, theta1 = theta1)$d.n, type = "b", ylab = expression(d["5j"]), xlab = "j")
lines(ARMAtoMA(ar = phi1, ma = theta1, lag.max = n), col = "blue", type = "b", lty = 2)

# Now look at ARMA(1, 1)
phi1 <- 0.5
theta1 <- -0.25
sig.sq.w <- 1
n <- 10
plot(solve.innov(n, phi1 = phi1, theta1 = theta1)$d.n, type = "b", ylab = expression(d["5j"]), xlab = "j")
lines(ARMAtoMA(ar = phi1, ma = theta1, lag.max = n), col = "blue", type = "b", lty = 2)

# Let's just look at the coefficients
solve.innov(n, phi1 = phi1, theta1 = theta1)$D
# What's happening as n gets bigger?
