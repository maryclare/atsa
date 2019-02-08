###################################
# Author: Megan Gelsinger
# Date: 2/8/19
# Title: STSCI 4550 Lab Session 3 
###################################

# In this lab session, we are going to learn about the "array" data structure.  This would
# have been a useful data structure for HW2 and I'd like to go over it with everyone
# before we more further into the semester.  We are also going to learn how to fit ARMA models,
# plot their ACFs, and how to overlay ACFs on one another.  Lastly, we are going to do another
# R Markdown demo, this time on writing mathematical equations.

## Arrays ##
# An array is a really useful data structure which can have any number of specified dimensions.
# This contrasts vectors and matrices, which we have seen previously; vectors are one dimensional
# data structures, and matrices are two dimensional data structures.  We will go over examples of 
# one, two and three dimensional array specifications, but know you can always add more dimensions 
# as needed.

# To begin, how do we specify an array?
?array

# Okay, so let's just initialize a blank, one dimensional array of length 10
x_1 <- array(dim = 10)

# Now, let's put some data in the vector
x_1[] <- 1:10 

# Next up, let's initialize and fill and two dimensional array of size 4 x 3 
x_2 <- array(dim = c(4, 3)) 
x_2[, 1] <- rep(3, times = 4) 
x_2[2, ] <- c(2, 5, -1)
x_2 

# Finally, let's think about a three dimensional array
x_3 <- array(dim = c(30, 7, 3)) 
phis <- c(.1, -.3, .22)
set.seed(4550)
for (k in 1:length(phis)){ # for each phi value,
  for (j in 1:7){ # draw 7 simulations
    x_3[, j, k] <- arima.sim(list(order = c(1,0,0), ar = phis[k]), n = 30, sd = 1) # of a length 30 time series
  }
}

# Check: what is printed with each of these calls?  If you see what you expected, you should be 
# ready to use arrays!
x_3[,,1] 
x_3[,2,]


## ARMA ##
# Now, let's get back to our time series models.  Yesterday, we learned about ARMA models.
# In this lab, we want to learn how to simulate them and how to plot their 
# (simulated and theoretical) ACFs.

# To simulate, we use the same "arima.sim" function and change the order and coefficients passed
set.seed(5550)
arma_sim <- arima.sim(list(order = c(1, 0, 1), ar = 0.23, ma = -0.78), n = 200, sd = 1)
plot(arma_sim)

# We can get the ACF from the simulation 
acf(arma_sim)

# What if we want the theorectical ACF for the given parameter values?
lag.max <- 10 # need to specify maximum number lags to computer
t_acf_1 <- ARMAacf(ar = 0.23, ma = 0.78, lag.max = lag.max) 
t_acf_2 <- ARMAacf(ar = -0.56, ma = 0.22, lag.max = lag.max)

# Now let's plot these together and compare their behaviors
plot(0:lag.max, t_acf_1, type = "b", col = "red", ylim = range(t_acf_1, t_acf_2)) 
lines(0:lag.max, t_acf_2, type = "b", col = "blue")
legend("topright", legend = c("model 1", "model 2"), col = c("red", "blue"), lty = 1, pch = 1)
