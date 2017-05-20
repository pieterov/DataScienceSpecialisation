##############################################################
# QUIZ - Practical Machine Learning - WEEK 2
##############################################################

# QUESTION 1:
#----------------------------------------
# Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

# Which of the following commands will create non-overlapping
# training and test sets with about 50% of the observations
# assigned to each?

# Answer:
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

# Answer: First. Third seems also to work but includes diagnosis too.


# QUESTION 2:
#----------------------------------------
# Load the cement data using the commands:

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
# Color by each of the variables in the data set (you may find the cut2() function
# in the Hmisc package useful for turning continuous covariates into factors).
# What do you notice in these plots?

# Cement, BlastFurnaceSlag, FlyAsh, Water   
# Superplasticizer, CoarseAggregate, FineAggregate
# Age, CompressiveStrength

color.by.factor <- cut2(mixtures$FlyAsh, g=5)
qplot(1:1030, mixtures$CompressiveStrength, 
      col=color.by.factor)

FlyAsh
Superplasticizer

# Answer: There is a non-random pattern in the plot of 
# the outcome versus index that does not appear to be perfectly explained 
# by any predictor suggesting a variable may be missing.


# QUESTION 3:
#----------------------------------------
# Load the cement data using the commands:

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a histogram and confirm the SuperPlasticizer variable is skewed.
# Normally you might use the log transform to try to make the data more symmetric.
# Why would that be a poor choice for this variable?

hist(mixtures$Superplasticizer)
color.by.factor <- cut2(mixtures$Superplasticizer, g=5)

hist(log10(mixtures$Superplasticizer+10))

# Answer: There are a large number of values that are the
# same and even if you took the log(SuperPlasticizer + 1)
# they would still all be identical so the distribution
# would not be symmetric.

# Or: There are values of zero so when you take the log()
# transform those values will be -Inf.


# QUESTION 4:
#----------------------------------------
# Load the Alzheimer's disease data using the commands:

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL.
# Perform principal components on these variables with the preProcess() function
# from the caret package. Calculate the number of principal components needed to
# capture 90% of the variance. How many are there?

training.il <- training[, grepl( "^IL" , names(training) )]
preProc <- preProcess(training.il, method=c("center", "scale", "pca"), thres = 0.8)
print(preProc)

#prComp <- prcomp(training.il)
#prComp$rotation
#cumsum(prComp$sdev^2/sum(prComp$sdev^2)*100)

# Answer: 9 for 90%, 7 for 80%

# QUESTION 5:
#----------------------------------------
# Load the Alzheimer's disease data using the commands:

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors[, grepl("^IL", names(predictors))])
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors with variable names
# beginning with IL and the diagnosis. Build two predictive models, one using the
# predictors as they are and one using PCA with principal components explaining 80%
# of the variance in the predictors. Use method="glm" in the train function.
# What is the accuracy of each method in the test set? Which is more accurate?

# PCA
preProc <- preProcess(training[,-1], method = "pca", thres = 0.8)
trainPC <- predict(preProc, training[,-1])
trainPC <- cbind(training$diagnosis, trainPC)
names(trainPC) <- c("diagnosis", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
head(trainPC)
modelFit <- train(diagnosis ~ ., method = "glm", data = trainPC)

testPC <- predict(preProc, testing[,-1])
confusionMatrix(testing$diagnosis, predict(modelFit, testPC))

# Non-PCA
modelFit <- train(diagnosis ~ ., method = "glm", data = training)
confusionMatrix(testing$diagnosis, predict(modelFit, testing[,-1]))

# Answer: Non-PCA Accuracy: 0.65, PCA Accuracy: 0.72
