# this script is to compare the effcacy among ridge, lasso, elastic-net regression

# load libraries
library(glmnet)

# make up a dataset for ridge, lasso, and elastic-net regression
set.seed(42)

n <- 1000
p <- 5000
real_p <- 15

x <- matrix(rnorm(n*p), nrow = n, ncol = p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

train_rows <- sample(1:n, 0.66*n)# 2/3 of the data will randomly be in the training set
x.train <- x[train_rows,]
x.test <- x[-train_rows,]

y.train <- y[train_rows]
y.test <- y[-train_rows]

# start with ridge regression
# fit a model to the training data
alpha0.fit <- cv.glmnet(x.train, y.train, type.measure = "mse",
                        alpha = 0, family = "gaussian")

# cv means use cross validation to obtain the optimal values for lambda, and 10-fold cross validation by default
# use x.train to predict y.train
# mse which stands for mean squared error is used for cross-validation evaluation
# mean squared error = the sum of squared residuals / the sample size
# alpha to 0 is for ridge regression
# family to family = "gaussian" is for linear regression, "binomual" is for logistic regression

# apply alpha0.fit to the testing data
alpha0.predict <- predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx = x.test)

# lambda.1se is the value for lambda that resulted in the simplest model
# lambda.min would be the lambda that resulted in the smallest sum

# calculate the mean square error of the difference between the true values(in y.test)
# and the predicted values(in alpha0.predicted)
mean((y.test - alpha0.predict)^2) # = 14.88459

# lasso regression
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure = "mse",
                        alpha = 1, family = "gaussian")

alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx = x.test)

mean((y.test - alpha1.predicted)^2) # = 1.184701

# so lasso regression is much better than ridge regression

# elastic-net regression
alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0.5, family="gaussian")

alpha0.5.predicted <- predict(alpha0.5.fit, s=alpha0.5.fit$lambda.1se, newx = x.test)

mean((y.test - alpha0.5.predicted)^2) # =1.225528

# need to try more alpha
list.of.fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  list.of.fits[[fit.name]] <- 
    cv.glmnet(x.train, y.train, type.measure = "mse", alpha = i/10, family="gaussian")
}

# the lasso regression is still the best

results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha",i/10)
  
  predicted <- 
    predict(list.of.fits[[fit.name]],
            s=list.of.fits[[fit.name]]$lambda.1se, newx = x.test)
  
  mse <- mean((y.test-predicted)^2)
  
  temp <- data.frame(alpha = i/10, mse=mse, fit.name = fit.name)
  
  results <- rbind(results, temp)
}
results
