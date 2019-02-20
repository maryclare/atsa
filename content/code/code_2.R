rm(list = ls())

# Let's look at a new example time series data set!
# This is some experimental data from Cowpertwaite and Metcalfe (2009)
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/wave.dat"
# It is a time series of wave height measurements in mm from a wave machine over
# a ~40 second interval
# We're going to play around with it because it's a good example
# of a stationary time series
x <- ts(data = read.table (www, header=T)[, 1])
n <- length(x)

plot(x, ylab = "Wave Height (mm)")

# First, let's consider Yule-Walker estimation of an AR-p model
# What order should we choose? 
# Let's look at the PACF
acf(x, type = "partial")
abline(v = 13, lty = 3, col = "gray")
# Look like the PACF suggests an order of p = 12
p <- 12

# What do we need to perform  estimation? We're going to need the 
# autocovariance function
gamma.hat <- acf(x, type = "cov", plot=FALSE, lag.max = p)$acf

# Based on the autocovariance function, we'll need our
# estimated A_p matrix and b_p vector
A.p.h <- matrix(nrow = p, ncol = p)
b.p.h <- matrix(nrow = p, ncol = 1)

for (i in 1:p) {
  b.p.h[i] <- gamma.hat[i + 1]
  for (j in 1:p) {
    A.p.h[i, j] <- gamma.hat[abs((i - j)) + 1]
  }
}
phi.h.yw <- solve(A.p.h)%*%b.p.h
mu.x.h.yw <- mean(x)
sig.sq.w.h.yw <- (gamma.hat[1] - t(phi.h.yw)%*%b.p.h)

# We could just use the Yule-Walker function to do this
r.yw <- ar.yw(x, aic = FALSE, order.max = p, demean = TRUE)

cbind(phi.h.yw, r.yw$ar)
c(mu.x.h.yw, r.yw$x.mean)
c(sig.sq.w.h.yw, r.yw$var.pred)
c(sig.sq.w.h.yw, r.yw$var.pred*(n - p - 1)/length(x))

# What about approximate method of moments estimation for MA(q)?
est.innov <- function(x) {
  n <- length(x)
  gamma.hat <- acf(x, type = "cov", plot=FALSE, lag.max = n-1)$acf
  D <- matrix(nrow = n-1, ncol = n-1)
  v <- rep(NA, n)
  gamma.x.0 <- gamma.hat[1]
  v[1] <- gamma.x.0
  for (i in 1:(n - 1)) {
    for (j in 0:(i-1)) {
      D[i, i - j] <- gamma.hat[i - j + 1]
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

# This gives us estimates of the vector d_n, which in turn gives us an estimate of psi
psi.hat <- est.innov(x)$d.n
plot(psi.hat, type = "l", ylab = expression(hat(psi)))
# We can then back out the MA parameters, but we won't do that here since
# you're probably never going to need to do approximate method of moments estimation
# for MA(q), ARMA(p, q) models in practice


