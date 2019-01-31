###################################
# Author: Megan Gelsinger
# Date: 2/1/19
# Title: STSCI 4550 Lab Session 2 
###################################

# In this lab session, we are going to learn how to simulate from AR and MA time series.
# We have two options for simulating time series - use a built-in function or create our own.
# We will cover both options today in lab, since you will be responsible for both on HW2. 

### AR Processes ###
## Option 1: Use a Built-In Function ##
# Our book introduces the function "arima.sim".  What is the function, where is it located,
# what are the arguments, and what does it return?
?arima.sim 

# Now that we know how to use the function, let's run a simulation.
# Make sure to always start any simulation by setting the seed!
# This guarantees both you and anyone using your code will get the same results every time.
set.seed(4550) 
ar.sim.1 <- arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = 150, sd = 1) 

# What does this time series and its (auto)correlation structure look like?
plot(ar.sim.1)  
acf(ar.sim.1) 
pacf(ar.sim.1) 

# Let's investigate a negative coefficient value in our AR(1) process
set.seed(5550) 
ar.sim.2 <- arima.sim(list(order = c(1, 0, 0), ar = -0.7), n = 150, sd = 1) 

# Compare plots of the two...
# in side-by-side-plots...
par(mfrow = c(2, 1)) 
plot(ar.sim.1, ylab = "x", main = (expression("AR(1): "*phi*" = +.7"))) 
plot(ar.sim.2, ylab = "x", main = (expression("AR(1): "*phi*" = -.7")))
# or in the same plot. (Hint: HW)
par(mfrow = c(1,1))
plot(ar.sim.1, ylab = "x", main = "AR(1)")
lines(ar.sim.2, col = "red")
legend("topright", legend = c(expression(phi*"= +.7"), (expression(phi*"= -.7"))), col = c("black", "red"), lty = 1)


## Option 2: Create Our Own Simulation By Hand ##
# We know the model equation for an AR(p) process, which means we can generate/simulate
# it on our own.  This helps reinforce what goes on in AR processes.
# First, we need to set our seed.
set.seed(6550)
# Next, we need to get the necessary values/vectors set.
w <- rnorm(n = 150) 
x <- vector(length = 150) 
x[1] <- rnorm(n = 1) 
# Now, we simulate our AR(1) process and update our x vector (our time series)
for(t in 2:150){ 
  x[t] <- 0.7 * x[t-1] + w[t]
}
# That's it! We have simulated an AR(1) process by hand!

# Let's look at the plot and compare it to ar.sim.1
par(mfrow = c(1,1))
plot(ar.sim.1, ylab = "x", main = "AR(1)")
lines(x, col = "red")
legend("topright", legend = c("arima.sim", "by hand"), col = c("black", "red"), lty = 1)  

# What if we wanted to simulate multiple, say 100, AR(1) time series.  
# How would we need to modify our code (option 1 and option 2)? (Hint: HW)
# For option 1:
set.seed(4550)
sim.ts.ar <- matrix(nrow = 150, ncol = 100) 
for (s in 1:100){
  sim.ts.ar[, s] <- arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = 150, sd = 1) 
}
# For option 2: Think about it!
# For different phi values: Think about it! (Hint: HW)


### MA Processes ###
## Option 1: Use a Built-In Function ##
ma.sim.1 <- arima.sim(list(order = c(0, 0, 1), ma = 0.7), n = 150, sd = 1) 

# What does this time series and its (auto)correlation structure look like?
plot(ma.sim.1)  
acf(ma.sim.1) 
pacf(ma.sim.1)

# Let's investigate a negative coefficient value in our AR(1) process
set.seed(5550) 
ma.sim.2 <- arima.sim(list(order = c(0, 0, 1), ma = -0.7), n = 150, sd = 1) 

# Compare plots of the two in side-by-side-plots
par(mfrow = c(2, 1)) 
plot(ma.sim.1, ylab = "x", main = (expression("MA(1): "*theta*" = +.7"))) 
plot(ma.sim.2, ylab = "x", main = (expression("MA(1): "*theta*" = -.7")))

## Option 2: Create Our Own Simulation By Hand ##
# This is a homework problem! #

### BONUS: Explosive AR model ###
# Why do we require |phi| < 1? What happes if |phi| >= 1?
# Let's adapt our simulation study to see
set.seed(6550)

w <- rnorm(n = 150) 
x <- vector(length = 150) 
x[1] <- rnorm(n = 1) 
phi <- 1 

for(t in 2:150){ 
  x[t] <- phi * x[t-1] + w[t]
}

par(mfrow = c(2,1))
# Stationary AR(1) process
plot(ar.sim.1, ylab = "x", main = (expression("AR(1): "*phi*" = +.7"))) 
# Explosive (non-stationary) AR (1) process
plot(ts(x), ylab = "x", main = paste("AR(1): ",expression(phi),"=", phi))

