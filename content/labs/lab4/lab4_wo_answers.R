###################################
# Author: Megan Gelsinger
# Date: 2/15/19
# Title: STSCI 4550 Lab Session 4 
###################################

# In this lab session, we are going to review how the ACF/PACF plots look for our three
# basic time series models, AR(p), MA(q), and ARMA(p,q) and why they look the way they do.
# These plots will help give us an intuitive sense of the order of our time series models
# when we are given raw data and no other information. We will formally discuss order estimation
# next week in lecture.  In this lab, we are also going to learn how to fit a time series 
# model to time series data, estimate the order and coefficients, make predictions/forecasts,
# and plot these predictions.  We have a lot to do, so let's get started!

set.seed(5550)

## Order estimation from ACF/PACF plots ##
par(mfrow = c(2, 1))
# AR(p) #
# Do we see the behavior we would expect in the ACFs/PACFs of the AR(1)?
acf(arima.sim(list(order = c(1, 0, 0), ar = .7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("AR(1): ", phi[1], " =+.7")))
acf(arima.sim(list(order = c(1, 0, 0), ar = -.7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("AR(1): ", phi[1], " =-.7"))) 
acf(arima.sim(list(order = c(1, 0, 0), ar = .7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("AR(1): ", phi[1], " =+.7")), type = "partial") 
acf(arima.sim(list(order = c(1, 0, 0), ar = -.7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("AR(1): ", phi[1], " =-.7")), type = "partial")

# Now, what if we have p>1?
acf(arima.sim(list(order = c(2, 0, 0), ar = c(.7, .2)), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("AR(2): ", phi[1], " =+.7, ", phi[2], " =+.2"))) 
acf(arima.sim(list(order = c(2, 0, 0), ar = c(.7, .2)), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("AR(2): ", phi[1], " =+.7, ", phi[2], " =+.2")), type = "partial")  

# MA(q) #
# Do we see the behavior we would expect in the ACFs/PACFs of the MA(1)?
acf(arima.sim(list(order = c(0, 0, 1), ma = .7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("MA(1): ", theta[1], " =+.7"))) 
acf(arima.sim(list(order = c(0, 0, 1), ma = -.7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("MA(1): ", theta[1], " =-.7"))) 
acf(arima.sim(list(order = c(0, 0, 1), ma = .7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("MA(1): ", theta[1], " =+.7")), type = "partial") 
acf(arima.sim(list(order = c(0, 0, 1), ma = -.7), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("MA(1): ", theta[1], " =-.7")), type = "partial") 

# Now, what if we have p>1?
acf(arima.sim(list(order = c(0, 0, 2), ma = c(.7, .3)), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("MA(2): ", theta[1], " =+.7, ", theta[2], " =+.3"))) 
acf(arima.sim(list(order = c(0, 0, 2), ma = c(.7, .3)), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("MA(2): ", theta[1], " =+.7, ", theta[2], " =+.3")), type = "partial") 

# ARMA(p.q) #
# Do we see the behavior we would expect in the ACFs/PACFs of the ARMA(1, 1)?
acf(arima.sim(list(order = c(1, 0, 1), ar = -.31, ma = .56), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("ARMA(1, 1): ", phi[1], " = -.31, ", theta[1], " =+.7"))) 
acf(arima.sim(list(order = c(1, 0, 1), ar = -.31, ma = .56), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("ARMA(1, 1): ", phi[1], " = -.31, ", theta[1], " =+.7")), type = "partial") 

# Now, what if we have p,q>1?
acf(arima.sim(list(order = c(2, 0, 3), ar = c(-.31, -.56), ma = c(.56, .13, .73)), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("ARMA(2, 3)"))) 
acf(arima.sim(list(order = c(2, 0, 3), ar = c(-.31, -.56), ma = c(.56, .13, .73)), n = 300, sd = 1), lag.max = 12, 
    main = expression(paste("ARMA(2, 3)")), type = "partial") 

# Take-away: ACFs/PACFs useful for estimating order of AR(p)/MA(q) models, NOT ARMA(p,q)

# Now, can you guess a few? (Main point: this isn't always easy!) #
acf(arima.sim(list(order = c(0, 0, 1), ma = .05), n = 300, sd = 1), lag.max = 12, 
    main = "") 
acf(arima.sim(list(order = c(0, 0, 1), ma = .05), n = 300, sd = 1), lag.max = 12, 
    main = "", type = "partial")

acf(arima.sim(list(order = c(1, 0, 1), ar = -.1, ma = .95), n = 300, sd = 1), lag.max = 12, 
    main = "") 
acf(arima.sim(list(order = c(1, 0, 1), ar = -.1, ma = .95), n = 300, sd = 1), lag.max = 12, 
    main = "", type = "partial")

acf(arima.sim(list(order = c(4, 0, 0), ar = c(.5, -.1, .22, .26)), n = 300, sd = 1), lag.max = 12, 
    main = "") 
acf(arima.sim(list(order = c(4, 0, 0), ar = c(.5, -.1, .22, .26)), n = 300, sd = 1), lag.max = 12, 
    main = "", type = "partial") 

acf(arima.sim(list(order = c(2, 0, 0), ar = c(.7, -.2)), n = 300, sd = 1), 
    lag.max = 12, main = "")
acf(arima.sim(list(order = c(2, 0, 0), ar = c(.7, -.2)), n = 300, sd = 1), 
    lag.max = 12, main = "", type = "partial")


## Fitting and Predicting AR(p) ##
# Let's take a look at our data set.
# The first question we should ask is, is it stationary (visual check only)?
austres
par(mfrow = c(1, 1))
plot(austres)
par(mfrow = c(2, 1))
acf(austres)
pacf(austres)

# How can we get a stationary time seris? (Don't worry about what this transformation is. 
# We will learn about it in the future.  Just accept that it is what we need to do.)
plot(austres)
plot(diff(austres))
d_austres <- diff(austres)

# Okay, now that we have a stationary time series, what model should we apply?
acf(d_austres)
pacf(d_austres) 

# Let's fit our AR(1) model.  There are multiple ways of doing this.  One can estimate the
# coefficient values by hand, as discussed in lecture.  You will also compute the estimates
# by hand on your homework.  It turns out, one can also let R do the estimation/fitting.
ar_model <- arima(d_austres, order = c(1, 0, 0))
ar_model
names(ar_model)
ar_model$coef

# Model checking:
# Coefficients significant? ---> stay tuned
# Residuals white noise?
par(mfrow = c(1,1))
plot(ar_model$residuals) 

# Since we are statisfied with our model, lets make some predictions using a built in 
# function in R.  Let's predict n = 8 in the future, and retain their standard errors
# so we can display confidence bands for our predictions. 
pred <- predict(ar_model, n.ahead = 8, se.fit = TRUE) 
t_range <- range(time(d_austres))
plot(d_austres, xlim = c(t_range[1], t_range[2] + 8/4))
abline(h = ar_model$coef[2])
lines(seq(t_range[2], t_range[2] + 8/4, by = 0.25), 
      y = c(d_austres[length(d_austres)], pred$pred), col = "blue")
lines(seq(t_range[2], t_range[2] + 8/4, by = 0.25), 
      y = c(d_austres[length(d_austres)], pred$pred + qnorm(0.975)*pred$se), col = "blue", lty = 2)
lines(seq(t_range[2], t_range[2] + 8/4, by = 0.25), 
      y = c(d_austres[length(d_austres)], pred$pred - qnorm(0.975)*pred$se), col = "blue", lty = 2)

