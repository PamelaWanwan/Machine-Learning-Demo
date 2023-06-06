# this script is to perform logistic regression using online data

# load packages
library(ggplot2)
library(cowplot)

# read-in data from UCI machine learning repository
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)

head(data) # we need to add column names
# details can be found at http://archive.ics.uci.edu/ml/datasets/Heart+Disease

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

# check the data structure
str(data)
# sex, cp, fbs, restecg, exang, and slope are supposed to be a factor
# ca and thal have the level"?", which supposed to be NA

# do some cleaning up
data[data == "?"] <- NA
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

# remove "NA"
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)

#we want to exclude variables that only have 1 or 2 samples in
#a category since +/- one or two samples can have a large effect on the
#odds/log(odds)

xtabs(~ hd + sex, data=data)
xtabs(~ hd + cp, data=data)
xtabs(~ hd + fbs, data=data)
xtabs(~ hd + restecg, data=data)
xtabs(~ hd + exang, data=data)
xtabs(~ hd + slope, data=data)
xtabs(~ hd + ca, data=data)
xtabs(~ hd + thal, data=data)

# perform logistic regression using one variable
logistic <- glm(hd~sex, data = data, family = "binomial")
summary(logistic)
## (Intercept)  -1.0438     0.2326  -4.488 7.18e-06 ***
##   sexM        1.2737     0.2725   4.674 2.95e-06 ***
female.log.odds <- log(25/71)#-1.0438 equals to the inrercept
male.log.odds.ratio <- log((112/89)/(25/71))# 1.2737 equals to the sexM

# calculate McFadden's Pseudo R2
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null
# R2 = 0.05812569
# calculate p-value using chi-square method
1 - pchisq(2*(ll.proposed - ll.null), df=1)
# 1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

# visualization
predicted.data <- data.frame(probability.of.hd=logistic$fitted.values,
                             sex=data$sex)
head(predicted.data)

ggplot(data = predicted.data, aes(sex, probability.of.hd)) +
  geom_point(aes(color=sex),size=4) + 
  xlab("Sex") +
  ylab("predicted probability of getting heart disease")

xtabs(~ probability.of.hd + sex, data=predicted.data)

# more complicated model
logistic <- glm(hd~., data = data, family = "binomial")
summary(logistic)

# R2 and p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

(ll.null - ll.proposed) / ll.null

1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

# plot the data
predicted.data <- data.frame(
  probability.of.hd=logistic$fitted.values,
  hd=data$hd)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

ggsave("heart_disease_probabilities.pdf")
