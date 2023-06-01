# this script is to show a design matrix example

# create labels for the mutant mice
Type <- factor(c(rep("Control",times = 4),
                 rep("Mutant", times = 4)))

# enter the weights for the control mice
Weight <- c(2.4, 3.5, 4.4, 4.9, 1.7, 2.8, 3.2, 3.9)
# enter the size for the control mice
Size <- c(1.9, 3, 2.9, 3.7, 2.8, 3.3, 3.9, 4.8)

# construct a desigh matrix
model.matrix(~ Type+Weight)
# y= control intercept + mutant offset + slope

# fit a linear model
model <- lm(Size~Type+Weight)
summary(model)

#p-value: 0.003367 it is better than y = the overall mean
#              Pr(>|t|)   
# (Intercept)  0.88463   
# TypeMutant   0.00230 ** it is better than if we removed the mouse type parameter(simple linear regression)
#  Weight      0.00256 ** it is better than if we removed the weight parameter(t-test)

# what if we want to compare experiments done by different labs
# generate labels
Lab <- factor(c(rep("A", times = 6),
                rep("B", times = 6)))
Type <- factor(c(rep("Control", times = 3),
                 rep("Mutant", times = 3),
                 rep("Control", times = 3),
                 rep("Mutant", times = 3)))

# type in all the expression values
Expression <- c(1.7, 2, 2.2,
                3.1, 3.6, 3.9,
                0.9, 1.2, 1.9,
                1.8, 2.2, 2.9)

# construct design matrix
model.matrix(~Lab+Type)
# y=labA control mean + lab B offset + difference(mutant-control)

# do linear regression
batch.lm <- lm(Expression~Lab+Type)
summary(batch.lm)

#p-value: 0.0007342 # compares to y= overall mean
#            Pr(>|t|)    
# (Intercept)  6.6e-06 ***
#  LabB        0.006250 ** 
#  TypeMutant  0.000956 *** compares to y=labA control mean + lab B offset
