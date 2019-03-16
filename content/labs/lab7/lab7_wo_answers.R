###################################
# Author: Megan Gelsinger (Maryclare Griffin)
# Date: 3/15/19
# Title: STSCI 4550 Lab Session 7 
###################################

# In this lab demo, we are going to visually and quantitatively assess various time series
# for stationarity.  For those that do not pass the tests, we are going to apply appropriate
# transformations to achieve stationarity.  Lastly, we are going to fit time series models
# to the stationary time series. 

## Visual Inspections ##

# What are the ways in which time series can be non-stationary?

# What do the various types of non-stationarity look like?  Can you identify the types below?
f <- function(){
  set.seed(123)
  likes <- arima.sim(120, model = list("order" = c(1, 0, 0), ar = 0.5))
  neutrals <- ts(1:100)*1 + rnorm(100, sd = 2)
  dislikes <- arima.sim(100, model = list("order" = c(1, 0, 1), ar = -0.25, ma = 0.36))
  return(list(likes = likes, neutrals = neutrals, dislikes = dislikes))
}
ex_models <- f()
par(mfrow = c(2, 2))
plot(AirPassengers, main = "A") # n.s. in mean and variance
plot(ex_models$likes, main = "B") # stationary
plot(ex_models$neutrals, main = "C") # not super clear, perhaps n.s. in mean
plot(ex_models$dislikes, main = "D") # stationary

# Now that we have guesses, let's try and investigate the stationarity properties more thoroughly.
# To do this, we will bin the data and compute the mean and variance within in bin.  If the
# mean and variance within each bin are approximately the same, then we (visually) have stationarity. 
binned.mv.plot <- function(x, title, bin.size = 30) {
  
  bins <- floor(0:(length(x) - 1)/bin.size) 
  small.bins <- which(table(bins) == min(table(bins)) & !table(bins) == max(table(bins)))
  bins[bins == small.bins - 1] <- NA
  
  bin.means <- aggregate(as.numeric(x), list("bins" = bins), mean)$x
  bin.vars <- aggregate(as.numeric(x), list("bins" = bins), var)$x
  
  par(mfrow = c(1, 2))
  plot(bin.means, type = "b", xlab = "Bin", ylab = "Mean", ylim = range(x), main = title)
  plot(bin.vars, type = "b", xlab = "Bin", ylab = "Variance", ylim = c(0, max(bin.vars)+10), main = title)
}
binned.mv.plot(AirPassengers, "A", bin.size = 12) 
binned.mv.plot(ex_models$likes, "B", bin.size = 10) 
binned.mv.plot(ex_models$neutrals, "C", bin.size = 6) 
binned.mv.plot(ex_models$dislikes, "D", bin.size = 8)  

## Quantitative Inspection ##
library(forecast) # for 'ndiffs'

# Let's perform some hypothesis tests to assess whether or not the time series are stationary.
# If they aren't stationary, how many differences do we need to take? 
# Think about the results before you execute the commands! You should have a good idea based on the exploratory plots we made above. 
?ndiffs 
# B
ndiffs(ex_models$likes, alpha = 0.05, test = "adf", type = "level"); ndiffs(ex_models$likes, alpha = 0.05, test = "pp", type = "level")
# C
ndiffs(ex_models$neutrals, alpha = 0.05, test = "adf", type = "level"); ndiffs(ex_models$neutrals, alpha = 0.05, test = "pp", type = "level")
# D
ndiffs(ex_models$dislikes, alpha = 0.05, test = "adf", type = "level"); ndiffs(ex_models$dislikes, alpha = 0.05, test = "pp", type = "level") 
# A
ndiffs(AirPassengers, alpha = 0.05, test = "adf", type = "level"); ndiffs(AirPassengers, alpha = 0.05, test = "pp", type = "level") 

## Transforming Data to Stationary Time Series ##
# Non-stationarity in the mean is removed by taking differences. The number of differences to 
# take is determined by the ndiffs() output.
diff_neutrals <- diff(ex_models$neutrals, lag = 1)
ndiffs(diff_neutrals, alpha = 0.05, test = "adf", type = "level"); ndiffs(diff_neutrals, alpha = 0.05, test = "pp", type = "level") 
diff_AP <- diff(AirPassengers, lag = 1)
ndiffs(diff_AP, alpha = 0.05, test = "adf", type = "level"); ndiffs(diff_AP, alpha = 0.05, test = "pp", type = "level") 
par(mfrow = c(2, 1))
plot(diff_neutrals);plot(diff_AP)

# Note: Differencing on its own does not remove the non-stationarity in the variance! To remove this,
# we need to apply a transformation to the data.  Usually, a log() or a sqrt() transformation 
# does the trick.  The ndiffs() function will NOT indicate non-stationarity in the variance, 
# nor will it tell you which transformation to take.
log_diff_AP <- diff(log(AirPassengers), lag = 1)
ndiffs(log_diff_AP, alpha = 0.05, test = "adf", type = "level"); ndiffs(log_diff_AP , alpha = 0.05, test = "pp", type = "level") 
par(mfrow = c(1,1))
plot(log_diff_AP)

## Fit ARIMA Time Series ##
# Now that we have stationary time series, we can fit models to them!
# Remember, the time series MUST be stationary before you fit models to them.
par(mfrow = c(3, 1))
plot(diff_neutrals)
acf(diff_neutrals)
acf(diff_neutrals, type = "partial")

# What type of model might we choose from these plots?

m1 <- arima(diff_neutrals, order = c(0, 0, 1))
m2 <- arima(ex_models$neutrals, order = c(0, 1, 1)) # no intercept 
m3 <- arima(ex_models$neutrals, order = c(0, 1, 1), include.mean = TRUE) # no intercept 
