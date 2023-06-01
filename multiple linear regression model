# this script is to do multiple linear regression in R
# setwd()

###### firstly, the simple linear regression####
# generate demo data
mouse.data <- data.frame(
  size = c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3),
  weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  tail = c(0.7, 1.3, 0.7, 2.0, 3.6, 3.0, 2.9, 3.9, 4.0))

mouse.data

# plot the data
plot(mouse.data$weight, mouse.data$size)

# fit a line to the data 
simple.regression <- lm(size ~ weight, data = mouse.data)
summary(simple.regression)
# Multiple R-squared:  0.6133
# p-value: 0.01256
# so the weight did a pretty good job predicting the size

# add the line that shows the least-squared fit on the plot
abline(simple.regression, col = "red", lwd = 2)

#### now the multiple linear regression ####

# plot all of the data
plot(mouse.data)
# we can see that both weight and tail are correlated with size
# weight and tail are also correlated which means they are similar
# and that we might not need both in our model

# fit a plane to the data 
multiple.regression <- lm(size ~ weight + tail, data = mouse.data)
summary(multiple.regression)
#            Pr(>|t|)  
#(Intercept)   0.3192  
#weight        0.4345  # it isn't significantly better than using tail alone to predict size
#tail          0.0219 *# using weight and tail is significantly better than using weight alone
# so we can just using tail lengths to predict size
