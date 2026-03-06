# --------------------------------------------------
# Homework 6 - Task 1
# Plotting 30-Day Mortality Rates for Heart Attack
# --------------------------------------------------

# Setting the working directory
setwd('/Users/sabreenaaleemnabeela/Desktop/Main Folder/Data Stewardship/HW6/data')

# -------------------------
# Loading and Inspecting Data
# -------------------------

# Reading dataset as character
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Viewing first few rows
head(outcome)

# -------------------------
# Converting Column 11 to Numeric
# -------------------------

# Column 11 = 30-Day Death Rate (Heart Attack)
outcome[, 11] <- as.numeric(outcome[, 11])

# Note:
# NAs introduced by coercion
# This is expected because some values are "Not Available".

# -------------------------
# Plotting Histogram
# -------------------------

hist(outcome[, 11],
     main = "30-Day Mortality Rates for Heart Attack",
     xlab = "Mortality Rate",
     ylab = "Frequency",
     col = "blue")

# --------------------------------------------------
# Validation Exercise
# --------------------------------------------------

# Reloading dataset to validate independently and Inspecting data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

# Coerce column 11 to numeric and plot histogram again
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11],
     main = "30-Day Mortality Rates for Heart Attack",
     xlab = "Mortality Rate",
     ylab = "Frequency",
     col = "blue")