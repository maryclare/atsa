###################################
# Author: Megan Gelsinger
# Date: 1/25/19
# Title: STSCI 4550 Lab Session 1 
###################################

# We are going to perform a univariate analysis using the "Iris" data set located in R
# If we just type "iris", the data set appears
iris 

# This looks like a data set of multiple variables and observations.  
# To confirm the actual size, we can extract the data set's dimension.
dim(iris) 

# This means, for 150 flowers, we recorded 5 variables worth of information.
# What are these variables?
names(iris) 

# What kinds of variables are these?
class(iris$Species) 
class(iris$Sepal.Length) 

# Hmmm, let's take a closer look at the "Species" column. This is our only categorical variable. 
# What kinds of species are we working with?
levels(iris$Species)

# How many of each type of flower do we have? 
table(iris$Species)

# What if we want to add a new categorical variable to this data, based on what we know? (Hint: HW)
# Let's say we know all Iris' with sepal length greater than 6 are blue.
Color <- vector(length = nrow(iris))
Color[which(iris$Sepal.Length > 6)] <- "blue"
Color[which(iris$Sepal.Length <= 6)] <- "red"

# Is this new variable of the correct class to use in regression?
class(Color)

# Let's correct the class.
Color <- as.factor(Color)

# Okay, now that we know a little bit about our data, let's think about an interesting research 
# question to guid our analysis.  
# RESEARCH QUESTION: What is the relationship between the sepal length and the petal length of 
#                    the Iris virginica?

# We need to pare down our data to only include the "virginica" species
vir_data <- iris[iris$Species == "virginica", ] 
vir_data 

# Now that we have our data, let's take a look at it.  This will give us intuition about the answer
# to our research question which will help us assess if our output makes sense. Mistakes when coding 
# happen, so going into your analysis with some intuition is always helpful!
plot(vir_data$Sepal.Length, vir_data$Petal.Length)

# Let's make the plot a little nicer.
plot(vir_data$Sepal.Length, vir_data$Petal.Length, xlab = "Sepal Length", ylab = "Petal Length", 
     main = "Flower: Iris virginica")

# If you want to go crazy...not recommended for this analysis
plot(vir_data$Sepal.Length, vir_data$Petal.Length, xlab = "Sepal Length", ylab = "Petal Length", 
     main = "Flower: Iris virginica", col = "blue", pch = 2, lwd = 2)
plot(vir_data$Sepal.Length, vir_data$Petal.Length, xlab = "Sepal Length", ylab = "Petal Length", 
     main = "Flower: Iris virginica", col = "blue", pch = 2, cex = 2)

# Now that we have constructed our visual, let's run our actual analysis!
lm(Petal.Length ~ Sepal.Length, data = vir_data)

# We want to retain the output returned from the call to lm().  There is actually more than what
# we just saw! Let's assign the output to a variable.
lm.out <- lm(Petal.Length ~ Sepal.Length, data = vir_data) 

# What else does our output contain?
names(lm.out)

# While there is a lot of information contained in "lm.out," let's focus on answering our research question.
# To get the most important information for answering our question, we will look at the model summary.
summary(lm.out) 
sum.lm.out <- summary(lm.out) 

# The summary lists the coefficient estimates and their standard errors.  What if we are interested
# in confidence intervals for the parameters?
# We can calculate these confidence intervals by hand for 95% and 80%.
c(sum.lm.out$coefficients[1, 1] - (qt(0.975, sum.lm.out$df[2]) * sum.lm.out$coefficients[1,2]), 
  sum.lm.out$coefficients[1, 1] + (qt(0.975, sum.lm.out$df[2]) * sum.lm.out$coefficients[1,2]))
c(sum.lm.out$coefficients[1, 1] - (qt(0.90, sum.lm.out$df[2]) * sum.lm.out$coefficients[1,2]), 
  sum.lm.out$coefficients[1, 1] + (qt(0.90, sum.lm.out$df[2]) * sum.lm.out$coefficients[1,2]))

c(sum.lm.out$coefficients[2, 1] - (qt(0.90, sum.lm.out$df[2]) * sum.lm.out$coefficients[2,2]), 
  sum.lm.out$coefficients[2, 1] + (qt(0.90, sum.lm.out$df[2]) * sum.lm.out$coefficients[2,2]))

# Or, we can use a function. 
confint(lm.out, "(Intercept)", level = 0.95)
confint(lm.out, "(Intercept)", level = 0.80)

confint(lm.out, "Sepal.Length", level = 0.95)
confint(lm.out, "Sepal.Length", level = 0.80)


# Let's say, we think that the petal length depends on more than just the sepal length.  We can add another
# covariate to our model and see if the fit is any better.
# We will assess this fit via an F-test and AIC values. (Hint: HW)
lm.out.2 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = vir_data)

# F-test (via ANOVA)
anova(lm.out, lm.out.2) 

# We can also compare the models via their AIC values. 
# Does the conclusion here match the conclusion from the F-test?
aic.lm.out <- AIC(lm.out)
aic.lm.out
aic.lm.out.2 <- AIC(lm.out.2) 
aic.lm.out.2

# If using a small sample size, correct AIC by using AICc statistic
# I will write my own function for this
AICc <- function(aic, k, n){
  aicc <- aic - ((n + (2 * k))/n) + ((n + k)/(n - k - 2))
  return(aicc)
}
AICc(aic.lm.out, n = 50, k = 2) 
