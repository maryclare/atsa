###################################
# Author: Megan Gelsinger
# Date: 3/8/19
# Title: STSCI 4550 Lab Session 6 
###################################

# This demo will provide you with another example of a time series data set which lends itself
# well to a spectral analysis.  In this demo, we will also discuss ways of making the 
# periodogram more interpretable by de-trending the data, changing the scale of the periodogram,
# and smoothing. 

# The Data #
# This is some experimental data from Cowpertwaite and Metcalfe (2009)
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/wave.dat"
# It is a time series of wave height measurements in mm from a wave machine over
# a ~40 second interval
# We're going to play around with it because it's a good example
# of a stationary time series
x <- ts(data = read.table (www, header=T)[, 1])
n <- length(x)
plot(x, ylab = "Wave Height (mm)")


# De-trend (Center) the Data #
# Why might you think it is important to de-trend data before you fit a periodogram?

# Why do I have "(Center)" above?  Hint: It applies to the data we are working with in this example.

# Let's look at an example with a linear trend.
par(mfrow = c(1, 1))
plot(nhtemp)
# The periodogram without de-trending
par(mfrow = c(2, 1))
spectrum(nhtemp, log = "no", detrend = FALSE)
# The periodogram with de-trending (built-in)
spectrum(nhtemp, log = "no")

# Let's look at an example with a non-zero mean.
par(mfrow = c(1, 1))
plot(ldeaths)
abline(h = mean(ldeaths), col = "blue")
# The periodogram without de-trending (w/o centering)
par(mfrow = c(3, 1))
spectrum(ldeaths, log = "no", detrend = FALSE)
# The periodogram with de-trending (centering, built-in)
spectrum(ldeaths, log = "no", detrend = FALSE, demean = TRUE)
spectrum(ldeaths, log = "no")

# Let's return to our data set. 
par(mfrow = c(1, 1))
mean(x)
plot(x)
abline(h = mean(x), lty = 2, col = "blue")
par(mfrow = c(2, 1))
spectrum(x, log = "no", detrend = FALSE)
spectrum(x, log = "no")

# How do I know if there is de-trending built into the function I am calling?

# Note: Some functions in R which compute periodograms have a built in de-trending step.  
# In other words, you can simply pass in the raw data.  This is not always the case, though,
# so read the function's documentation before using!  As an alternative, always de-trend the 
# data yourself. 


# The Log-Scale Periodogram #
# Why might a person want to change the scale of his/her periodogram? 

spectrum(sunspots, log = "no")
spectrum(sunspots) # Ignore the blue line for now.

# Let's return to our data data
spectrum(x, log = "no")
spectrum(x)


# Smoothing the Periodogram - Nonparametric Approach#
# Why might a person want to smooth his/her periodogram?

# How might we smooth?  What do we normally do to smooth?

# We are going to focus on the Daniell kernel which is a centered moving average kernel.
# Ex: hat{x_t} = (x_{t-2} + x_{t-1} + x_t + x_{t+1} + x_{t+2})/5
?kernel
# Look at the output below.  What does the "0" do?  What does the "5" do?
kernel("daniell", c(0, 5)) 

# Let's try another. What does the "2" do? What does the "5" do?
kernel("daniell", c(2, 5)) # write this one on the board (hats above all x's)

# These examples show that we have a lot of control over the amount of smoothing
# we apply to our periodogram.  Let's visualize these now!
kern_1 <- kernel("daniell", c(0, 5)) 
kern_2 <- kernel("daniell", c(2, 5))
spectrum(x, kern = kern_1, log = "no")
spectrum(x, kern = kern_2, log = "no")

# Can we smooth too much?
kern_3 <- kernel("daniell", c(10, 10))
spectrum(x, kern = kern_1, log = "no")
spectrum(x, kern = kern_3, log = "no")

# Smoothing the Periodogram - Parametric Approach#
# We could also do an AR() fit to the spectrum.
par(mfrow = c(1, 1))
spec.ar(x, method = "yw", log = "no", order = 2)
spec.ar(x, method = "yw", log = "no") # Picks order using AIC
