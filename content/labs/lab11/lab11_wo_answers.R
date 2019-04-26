###################################
# Author: Megan Gelsinger 
# Date: 4/26/19
# Title: STSCI 4550 Lab Session 11 
###################################

# In this lab, we are going to learn how to change the priors used to fit the stochastic
# volatility model. 

library(astsa)
library(stochvol)

# Let's load in and visualize the data
data("nyse")
nyse <- nyse - mean(nyse)
n <- length(nyse)
to.use <- 1:(n - 300)
y <- nyse[to.use]

par(mfrow = c(1, 1))
plot(c(y), type = "l")

# Now let's simulate from the posterior of our simple stochastic volatility model 
# using the default priors
res <- svsample(y)

# We can also make forecasts with this model
n.ahead <- length((length(to.use) + 1):n)
pred <- predict(res, n.ahead)

plot(c(nyse), type = "l", col = "gray", xlim = c(length(nyse) - 2*n.ahead, length(nyse)), ylim = c(-1, 1)*0.06)
lines(y, col = "black")
lines((length(to.use) + 1):n, colMeans(pred$y), col = "blue", lwd = 2)
lines((length(to.use) + 1):n, apply(pred$y, 2, function(x){quantile(x, 0.025)}), col = "blue", lty = 2, lwd = 2)
lines((length(to.use) + 1):n, apply(pred$y, 2, function(x){quantile(x, 0.975)}), col = "blue", lty = 2, lwd = 2)

# Okay, now what if we wanted to adjust the number of draws we make from the posterior distribution?
?svsample
res2 <- svsample(y, draws = 500)  

pred2 <- predict(res2, n.ahead)

lines((length(to.use) + 1):n, colMeans(pred2$y), col = "purple", lwd = 2)
lines((length(to.use) + 1):n, apply(pred2$y, 2, function(x){quantile(x, 0.025)}), col = "purple", lty = 2, lwd = 2)
lines((length(to.use) + 1):n, apply(pred2$y, 2, function(x){quantile(x, 0.975)}), col = "purple", lty = 2, lwd = 2)

# What if we wanted to change the prior on the parameters?
# Note the defaults! 
?svsample
res3 <- svsample(y, priormu = c(0, 5))
head(res3$para)
hist(res$para[, 1])
plot(density(res$para[, 1]), col = "blue", lwd = 2, main = "mu")
lines(density(res3$para[, 1]), col = "red", lwd = 2)

res4 <- svsample(y, priorphi = c(5, 3))
plot(density(res$para[, 2]), col = "blue", lwd = 2, main = "phi")
lines(density(res4$para[, 2]), col = "red", lwd = 2)

res5 <- svsample(y, priorsigma = 2)
plot(density(res$para[, 3]), col = "blue", lwd = 2, main = "sigma")
lines(density(res5$para[, 3]), col = "red", lwd = 2)
