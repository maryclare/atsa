rm(list = ls())

library(astsa)
library(Hmisc) # For Lag function

data("soi") # Southern Oscillation Index, related to sea surface temperature
data("rec") # Number of new fish

X <- cbind(soi, rec)
X <- apply(X, 2, function(x) {x - mean(x)})

# Let's multivariate and univariate models with the same order
order <- 8
var <- ar(X, method = "yw", order.max = order,  
          aic = FALSE)
ar <- ar(X[, 2], method = "yw", order.max = order,  
         aic = FALSE)

n <- nrow(X)
n.ahead <- 100
plot(time(rec), X[, 2], xlim = range(c(time(soi), time(soi) + n.ahead/11)), type = "l")
lines(max(time(soi)) + 1:n.ahead/11, c(predict(var, n.ahead = n.ahead)$pred[, 2]), col = "blue")
lines(max(time(soi)) + 1:n.ahead/11, c(predict(ar, n.ahead = n.ahead)$pred), col = "red")

