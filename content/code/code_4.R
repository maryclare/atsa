# Let's review how we can compute the periodogram
# We have the function we used in the homework
scaled.periodogram <- function(x) {
  n <- length(x)
  # Get number of columns in our design matrix
  Z <- matrix(nrow = n, ncol = n)
  
  # First column is always the intercept!
  Z[, 1] <- 1
  for (i in 2:n) {
    if (i%%2 == 0) {
      Z[, i] <- cos(2*pi*floor(i/2)*1:n/n)
    } else {
      Z[, i] <- sin(2*pi*floor(i/2)*1:n/n)
    }
  }
  linmod <- lm(x~Z-1)
  
  # Let's record the coef magnitudes
  m <- ifelse(n%%2 == 0, n/2, (n - 1)/2 + 1)
  coef.mags <- numeric(m)
  for (i in 1:length(coef.mags)) {
    if (i == 1) {
      coef.mags[i] <- coef(linmod)[1]^2
    } else if (i == length(coef.mags) & n%%2 == 0) {
      coef.mags[i] <- coef(linmod)[length(coef(linmod))]^2
    } else {
      coef.mags[i] <- sum(coef(linmod)[1 + 2*(i - 2) + 1:2]^2)
    }
  }
  return(list("coef.mags" = coef.mags, "freqs" = 0:(m - 1)/n, "Z" = Z))
}

# These are all different ways of computing the periodogram that give the same answer!
par(mfrow = c(1, 1))
spectrum(x, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0 # We haven't talked about tapering in class, so set this to 0
         ) 
# Note - the default is fast=FALSE, log="yes", detrend=TRUE, and taper=0
lines(0:m/n, abs(fft(x)[1:(m + 1)])^2/n, col = "blue", lty = 2)
# * The spectrum function uses the fft function under the hood - we can compute
#   the same thing by applying the fft function directly. Note that the fft
#   function does return a magnitude for the intercept
# * Also - we only need the first 1:(m + 1) points, if you examine abs(fft(x))^2/n 
#   you'll see that the remaining points are redundant
# * We have to divide this by n to make the scaling match up, this can be software specific
sp <- scaled.periodogram(x)
lines(sp$freqs, sp$coef.mags*c(n, rep(n/4, 
                                      length(sp$coef.mags) - 1)), col = "red", lty = 3)



# Let's simulate some time series according to the model given by (1) in notes_6.pdf
# - Play around with this code and see how things change! Specifically, try different:
#   * Values of r
#   * Values of n
library(splines) 
# This is just to help us invent some spectral density functions that are
# smooth functions of the frequencies

# Number of periodic components
r <- 100
# Frequencies of periodic components
freqs <- sort(sample(seq(0, 0.5, length.out = 10000), r, replace = FALSE))

# Construct some sigma^2 values - we do it this way to make sure
# that as r gets bigger, the spectral density function will be continuous
sig.sqs <- bs(freqs, degree = 3, df = 10)%*%abs(rnorm(d))

par(mfrow = c(2, 2))
plot(freqs, sig.sqs, pch = 16, xlab = "Frequency",
     ylab = expression(sigma^2), xlim = c(0, 0.5), main = "True Spectral Density (SD)")

n <- 100
x <- numeric(n)
t <- 1:n
for (k in 1:r) {
  u <- rnorm(1, sd = sqrt(sig.sqs[k]))
  v <- rnorm(1, sd = sqrt(sig.sqs[k]))
  x <- x + u*sin(2*pi*freqs[k]*t) + v*cos(2*pi*freqs[k]*t)
}

plot(x, type = "l", xlab = "Time", ylab = "x", main = "Simulated Time Series")
# Examine the parametric periodogram estimate we get from the AIC-minimizing
# AR(p) model
spec.ar <- spec.ar(x, main = "Parametric SD Estimate")
mtext(gsub(") spectrum ", "", gsub("(", "", spec.ar$method, fixed = TRUE), fixed = TRUE), 3,
      line = 0.25)
acf(x, main = "Autocorrelation Function")

