rm(list = ls())

set.seed(1)

get.x <- function(n) {
  
  sig.w <- 2
  A <- 2
  phi <- 0.6*pi
  omega <- 1/50
  
  return(ts(A*cos(2*pi*omega*1:n + phi) + sig.w*rnorm(n)))
}

n <- 501

x <- get.x(n)

plot(x)
acf(x)
acf(x, type = "partial")

ps <- 0:5
qs <- 0:5
aics <- matrix(NA, nrow = length(ps), ncol = length(qs))

for (i in 1:length(ps)) {
  for (j in 1:length(qs)) {
    p <- ps[i]
    q <- qs[j]
    
    cat("p=", p, " q=", q, "\n")
    # arma.fit <- arima(x, order = c(p, 0, q))
    arma.fit <- arima(x, order = c(p, 0, q),
                                        optim.control = list(maxit = 500))
    if (arma.fit$code == 0) { # Only record AIC if the arma function converged
      aics[i, j] <- log(arma.fit$sigma2) + (n + 2*(p + q + 1))/n
    } else {
      cat("Did not converge!\n")
    }
  }
}
which.min <- which(aics == min(aics, na.rm = TRUE), arr.ind=TRUE)
ps[which.min[1]]; qs[which.min[2]]

plot(x)
time <- 0:n
lines(time, cos(2*pi*time/n), col = "blue", lwd = 2)
# Not oscillating as fast as our data
lines(time, cos(2*pi*5*time/n), col = "red", lwd = 2)
lines(time, cos(2*pi*10*time/n), col = "purple", lwd = 2)

plot(x)
lines(time, cos(2*pi*10*time/n), col = "purple", lwd = 2)
lines(time, cos(2*pi*10*time/n + 2), col = "red", lwd = 2)
lines(time, 5*cos(2*pi*10*time/n + 2), col = "blue", lwd = 2)

# This is looking pretty close!
A <- 2
omega <- 10/n
phi <- 2

plot(x)
lines(time, A*cos(2*pi*omega*time + phi), lwd = 2, col = "blue")

# If we knew omega and phi, we could have estimated A by regression
summary(lm(x~I(cos(2*pi*omega*1:n + phi))-1))

# If we know omega, there's a nice trig identity that can help us out!
# cos(a + b) = cos(a)*cos(b) - sin(a)sin(b)
plot(x)
lines(time, A*cos(2*pi*omega*time + phi), lwd = 2, col = "blue")
lines(time, A*cos(phi)*cos(2*pi*omega*time) - A*sin(phi)*sin(2*pi*omega*time), lwd = 2, col = "red")
# Then we can estimate A and phi if we know omega!
summary(lm(x~I(cos(2*pi*omega*1:n)) + I(sin(2*pi*omega*1:n))-1))

# We still don't know omega, so one thing we might consider is to consider a range of 
# values. How many regressors can we have?
Z <- matrix(nrow = n, ncol = n)
Z[, 1] <- 1
for (i in 2:n) {
  if (i%%2 == 0) {
    Z[, i] <- cos(2*pi*floor(i/2)*1:n/n)
  } else {
    Z[, i] <- sin(2*pi*floor(i/2)*1:n/n)
  }
}
linmod <- lm(x~Z-1)
summary(linmod)
plot(x)
lines(linmod$fitted.values, col = "blue")

# Let's look at the values!
# Intercept first
coef(linmod)[1]
# Now the most slowly varying sines and cosines
plot(Z[, 2], type = "l")
lines(Z[, 3])
coef(linmod)[2:3]
sum(coef(linmod)[2:3]^2)

# Faster moving
plot(Z[, 4], type = "l")
lines(Z[, 5])
coef(linmod)[4:5]
sum(coef(linmod)[4:5]^2)

coef.mags <- numeric((ncol(Z)-1)/2)
for (i in 1:length(coef.mags)) {
  coef.mags[i] <- sum(coef(linmod)[1 + 2*(i - 1) + 1:2]^2)
}
plot(c(coef(linmod)[1]^2, coef.mags), type = "l")
(which(coef.mags == max(coef.mags)) - 1)/n
omega

# This is the same as doing a fourier transform!
lines(abs(2*fft(x)[1:(floor(n/2))]/n)^2, type = "l", col = "red")

spectrum(x, log = "no")

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
spectrum(x, log = "no")
