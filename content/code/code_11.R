rm(list = ls())

library(astsa)
library(Hmisc) # For Lag function

data("soi") # Southern Oscillation Index, related to sea surface temperature
data("rec") # Number of new fish

# Both are observed monthly
head(time(soi))
head(time(rec))

par(mfrow = c(2, 1))
plot(soi)
plot(rec)

par(mfrow = c(1, 1))
ccf(c(soi), c(rec))

plot(soi, rec)
plot(Lag(soi, 1), rec)
plot(Lag(soi, 6), rec)

X <- cbind(soi, rec)
X <- apply(X, 2, function(x) {x - mean(x)})
var <- ar(X, aic = FALSE, order.max = 1,  
          method = "yw")


