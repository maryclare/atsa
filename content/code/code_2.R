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
acf(x, lag.max = 50)
abline(v = 27, lty = 3, col = "gray")
# This suggests an MA q model with q = 27
q <- 27
# With no AR terms, the thetas are the psis!
theta.hat <- psi.hat[1:q]
# Get the noise variance from computing E[x_t^2]
sig.sq.w.hat <- acf(x, lag.max = 0, type = "cov", plot = FALSE)$acf[1, 1, 1]/(1 + sum(theta.hat^2))
mu.x.hat <- mean(x) # Again, this is out moment estimator of the covariance

# What if we wanted to fit an ARMA model? Use p = 12, q = 27
# First, compute phis based on c = D phi, where c is the q+1,...,q+p terms of psi.hat
# and elements of D are elements of psi
c <- psi.hat[(q + 1):(p + q)]
D <- matrix(0, nrow = p, ncol = p)
for (j in 1:p) {
  for (i in 1:p) {
    if (i <= min(j, p)) {
      D[j, i] <- psi.hat[q + j - i]
    }
  }
}
phi.hat <- solve(D)%*%c

E <- matrix(0, nrow = q, ncol = p)
for (j in 1:q) {
  for (i in 1:p) {
    if (i <= min(j, p)) {
      if (j - i == 0) {
        E[j, i] <- 1
      } else {
        E[j, i] <- psi.hat[j - i]
      }
    }
  }
}
theta.hat <- psi.hat[1:q] - E%*%phi.hat

plot(phi.hat, type = "b")
plot(theta.hat, type = "b")
# These look pretty crazy...they are not very good estimators...
# We already knw that would be the case though

# Let's move on to maximum likelihood methods. These are implemented by the arima function
?arima

# The default fits the model using unconditional maximum likelihood
p <- 2
q <- 3
system.time(uml <- arima(x, order = c(p, 0, q), method = "ML"))
# Could try the easy thing instead
system.time(cls <- arima(x, order = c(p, 0, q), method = "CSS"))
# Also has a conditional-maximum likelihood method - 
# This throws out the first p+q observations but not the 
# log(r) term. We haven't talked about this, so we won't try it out
cbind(coef(uml), coef(cls))
# They look similar!

# We get an asymptotic variance covariance matrix 
uml$var.coef
# This is where our standard errors come from
sqrt(diag(uml$var.coef))
uml
# And - we get a noise variance estimate
uml$sigma2

# Let's try picking the ARMA(p, q) order!
ps <- 0:8
qs <- 0:8
aic <- aicc <- sic <- matrix(nrow = length(ps), ncol = length(qs))

for (i in 1:length(ps)) {
  cat("i=", i, "\n")
  for (j in 1:length(qs)) {
    p <- ps[i]
    q <- qs[j]
    uml <- arima(x, order = c(p, 0, q), method = "ML")
    sig.sq <- uml$sigma2
    aic[i, j] <- log(sig.sq) + (n + 2*(p + q + 1))/n
    aicc[i, j] <- log(sig.sq) + (n + p + q + 1)/(n - p - q - 1 - 2)
    sic[i, j] <- log(sig.sq) + (p + q + 1)*log(n)/(n)
  }
}

aic.pq <- which(aic == min(aic), arr.ind = TRUE)
aicc.pq <- which(aicc == min(aicc), arr.ind = TRUE)
sic.pq <- which(sic == min(sic), arr.ind = TRUE)
c(ps[aic.pq[1]], qs[aic.pq[2]])
c(ps[aicc.pq[1]], qs[aicc.pq[2]])
c(ps[sic.pq[1]], qs[sic.pq[2]])

# What if we really underfit the model?
uml <- arima(x, order = c(2, 0, 2), method = "ML")
# Can look at the residuals
plot(uml$residuals, ylab = "Residuals")
acf(uml$residuals, main = "Residuals")
acf(uml$residuals, type = "partial", main = "Residuals")
# Eeep! There's still a lot of temporal dependence left!