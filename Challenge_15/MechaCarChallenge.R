# Project: design a linear model that predicts the mpg of
# MechaCar prototypes using a number of variables within
# the MechaCar mpg data set.

# Which variables/coefficients provided a non-random amount
# of variance to the mpg values in the dataset?
#   
# Is the slope of the linear model considered to be zero?
# Why or why not?
# 
# Does this linear model predict mpg of MechaCar prototypes
# effectively? Why or why not?
  


# Load data set
mechacarTable = read.csv("MechaCar_mpg.csv")

# Linear Regression
# MPG is the dependent variable

mpg_regression <- lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle +
                       ground.clearance + AWD, data = mechacarTable)

mpg_regression_summary = summary(summary(lm(mpg ~ vehicle.length +
vehicle.weight + spoiler.angle + ground.clearance + AWD, data = mechacarTable)))

# Graph slopes
# https://stackoverflow.com/questions/17615791/plot-regression-line-from-multiple-regression-in-r
par(mfrow=c1,5))
termplot(mpg_regression)