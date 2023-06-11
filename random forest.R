# this script is to perform a random forest model

# load packages
library(ggplot2)
library(cowplot)
library(randomForest)

# using a heart disease data set from the machine learning repository
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header = F)
head(data)#do not contain colnames

# name the columns using information from the UCI website
colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain 
  # 1 = typical angina, 
  # 2 = atypical angina, 
  # 3 = non-anginal pain, 
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment 
  # 1 = upsloping 
  # 2 = flat 
  # 3 = downsloping 
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease 
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

head(data) 

# clean up the data
str(data)
data[data == "?"] <- NA_character_

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(data$hd == 0, "Healthy", "Unhealthy")
data$hd <- as.factor(data$hd)

str(data)

set.seed(42)

# impute NA
data.imputed <- rfImpute(hd~., data = data, iter = 6)
# we want to predict the hd columns by the data in all of the other columns
# iter=6 means rfImpute should build 6 random forests to estimate the missing values
# in theory, 4 to 6 is enough, more iters would not improve the estimates
# OOB(out-of-bag error rate)
table(is.na(data.imputed))

# build a random forest model
model <- randomForest(hd~., data = data.imputed, proximity = T)

model

# Type of random forest: classification
# if we use the random forest to predict weight or height, it would say "regression"
# if we omitted the thing the random forest was supposed to predict entirely, it would say "unsupervised"
# Number of trees: 500 this is the default value
# No. of variables tried at each split: 3
# classification trees have a default setting of the square root of the number of variables
# regression trees have a fefault setting of the number of variables divided by 3
# OOB estimate of  error rate: 16.83%, means that more than 80% of the OOB samples were correctly classified by the random forest
# confusion matrix tells the accuracy and sensitivity...

# by ploting the error rates, we can see if No. of variables tried at each split: 3 is enough
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))
head(oob.error.data)

ggplot(oob.error.data, aes(x=Trees, y=Error)) + 
  geom_line(aes(color=Type))

# try more trees and compare them
model <- randomForest(hd~., data = data.imputed, ntree = 1000,proximity = T)
model
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))
head(oob.error.data)

ggplot(oob.error.data, aes(x=Trees, y=Error)) + 
  geom_line(aes(color=Type))
# the line is stable even though we increased the number of trees
# so 500 is enough

# to determine whether No. of variables tried at each split: 3 is good enough
oob.values <- vector(length = 10)
for (i in 1:10) {
  tem.model <- randomForest(hd~., data = data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- tem.model$err.rate[nrow(tem.model$err.rate),1]
}
oob.values
# the fifth mtry got the lowest error rate 0.1683168
model <- randomForest(hd~., data = data.imputed, mtry=5,ntree = 1000,proximity = T)
model
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))
head(oob.error.data)

ggplot(oob.error.data, aes(x=Trees, y=Error)) + 
  geom_line(aes(color=Type))
 
# use the best random forest model to draw MDS plot with samples
# make distance matrix
distance.matrix <- as.dist(1-model$proximity)
# scale the project
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
# calculate the percentage of variation in the distance matrix that X and Y are account for
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
# format the data for ggplot
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)
# draw the graph for ggplot
ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
