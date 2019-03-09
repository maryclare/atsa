###################################
# Author: Megan Gelsinger
# Date: 3/1/19
# Title: STSCI 4550 Lab Session 5 
###################################

# This demo will show you how to import and specify data as a time series object.
# We will cover importing data from .xlsx and .txt files.  Similar techniques apply
# to importing data with other file extensions, however the exact syntax/functions required
# may be different.  If in doubt, Google the solution!

## File Type I - .txt ##
# Data in this format is often easy to import, which is why we are starting with it.
# The R function used to import .txt files is "read.table".
# Let's explore what we need to know about the data in order to properly read it into R.
?read.table

# Based on the help file, the primary attributes we need to know are 
# the file name, if the file contains a header, how the data is separated.  
# I have also had to adjust "dec," "na.strings," and "comment.char" when I have worked
# with .txt data.  Review your data carefully so that you can read it in properly!

# The data that we are going to use for this example is minute-by-minute data on household
# electricity consumption (for one house) between December 2006 and November 2010.  This
# gives us a total of 2075259 observations. Let's look at this data. 

# Now that we know how our data looks and how we should set the arguments in the import function,
# let's go ahead and import it!   
pc <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")

# Hmmm, why didn't that work?  It appears as though the data file doesn't exist...what is 
# going on? 

# It turns out, the data is saved in a specific folder on my computer and I have to tell
# R to go there to find it.  
getwd() 
setwd("~/Documents/Time Series TA (Spring 2019)/labs/5") 
getwd()

# Now, let's try importing the data again. 
pc <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")

# Does this look right?

# Now that we have our data, let's specify it as a time series object.  To do this,
# we will use the "ts" function.  What do we need to specify a time series object?
?ts

# Okay, let's specify our time series, paying attention to our frequency designation.
# Note, if you want to include the specific time of each point, you can do this by specifying
# the "start" value in your ts() call.  I opt not to do that here becuase it is rather complicated here.
pc_red <- pc[1:(60*24*365), 3] 
pc_ts_wk <- ts(pc_red, frequency = 10080) # weekly seasonality
plot(pc_ts_wk)
pc_ts_daily <- ts(pc_red, frequency = 1440) # daily seasonality
plot(pc_ts_daily)

# How could I modify this data set if I was interested in daily activity, or monthly activity?

## File Type II - .xlsx ##
# Data in this format can be harder to import, since data can be stored in multiple sheets
# and the headers are often times more than one row long.  Likewise, we are going to need
# to spend more time reviewing the arguments for reading in files of this type. 
# The R function used to import .xlsx files is "read_excel" located in the "readxl" package.
# Let's explore what we need to know about the data in order to properly read it into R.
library("readxl")
?read_excel

# Based on the help file, the primary attributes we need to know are 
# the file path, which sheet to import, if the column names are included, and the number
# of lines to skip before reading in the data.  Review your data carefully so that you can read it in properly!

# The data that we are going to use for this example is quaterly prices for the S&P 500 
# index from 1988 - 2018.  This gives us a total of 124 observations. Let's look at this data. 

# Now that we know how our data looks and how we should set the arguments in the import function,
# let's go ahead and import it and make it a time series!   
getwd()
sp <- read_xlsx("sp500.xlsx", sheet = 3, col_names = TRUE, skip = 5)
sp_ordered <- sp[order(sp$END), ]
sp_ts <- ts(sp_ordered[, "PRICE"], frequency = 4, start = c(1988, 1))
plot(sp_ts)
