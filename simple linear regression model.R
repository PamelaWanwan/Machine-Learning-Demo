# this script is to do simple linear regression in R
# setwd()

# generate a demo data
mouse.data <- data.frame(weight=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
                         size=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))
mouse.data

# plot the data on a x/y graph
plot(mouse.data$weight,mouse.data$size)

# do the regression
mouse.regression <- lm(size ~ weight, data = mouse.data)
# size ~ weight means the size are considered to be the Y values and weight are
# considered to be the x values.
# the linear model function then calculate the least sqaures estimates for the y-
# -intercept and the slope

summary(mouse.regression)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|) 
# (Intercept)   0.5813     0.9647   0.603 0.5658 
# weight        0.7778     0.2334   3.332 0.0126*
# 0.5813 is for the y-axis intercept
# 0.7778 is for the slope
# we want the p-value for weight to be less than 0.05
# Multiple R-squared:  0.6133
# F-statistic:  11.1 on 1 and 7 DF,  p-value: 0.01256

# add the regression line to our plot
abline(mouse.regression, col="blue")
