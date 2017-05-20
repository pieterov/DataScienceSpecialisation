##############################################################
# QUIZ - Practical Machine Learning - WEEK 3
##############################################################

# QUESTION 1:
#----------------------------------------
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# Create training and test sets
inTrain <- createDataPartition(y = segmentationOriginal$Case, 
                               p=0.7, list=FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]
dim(training); dim(testing)

set.seed(125)

library(rpart)
rpart1 <- rpart(Class~., data = training, 
                control = rpart.control(maxdepth=2))
rpart1

#a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
#b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
#c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
#d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

# Answer: 
# a) PS, b) WS, c) PS, d) Not possible to predict

# QUESTION 2:
#----------------------------------------
# If K is small in a K-fold cross validation is the bias in the estimate
# of out-of-sample (test set) accuracy smaller or bigger? If K is small 
# is the variance in the estimate of out-of-sample (test set) accuracy 
# smaller or bigger. Is K large or small in leave one out cross validation?

# Answer: The bias is larger and the variance is smaller. Under leave one 
# out cross validation K is equal to the sample size.

# QUESTION 3:
#----------------------------------------
library(pgmm); library(caret); set.seed(125)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

#Create training and test sets
inTrain <- createDataPartition(y=olive$Area, p=0.7, list=FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
dim(training); dim(testing)

modFit <- train(Area ~ ., method="rpart", data=training)
print(modFit$finalModel)

# Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

# Predicting new values
predict(modFit, newdata=newdata)

# Answer: 2.72973. It is strange because Area should be a qualitative
# variable - but tree is reporting the average value of Area as a
# numeric variable in the leaf predicted for newdata.

# QUESTION 4:
#----------------------------------------
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(1234)

fitMod <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, 
                    data=trainSA, method="glm", family="binomial")

missClass = function(values, prediction) {
        sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd, predict(fitMod, newdata=testSA))
missClass(trainSA$chd, predict(fitMod, newdata=trainSA))

# Answer: 0.31 and 0.27, resp. 

# QUESTION 5:
#----------------------------------------
library(ElemStatLearn); library(ggplot2)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

# Random forests
modelfit <- randomForest(y ~ ., data = vowel.train, 
                         importance = FALSE)

order(varImp(modelfit), decreasing = TRUE)

# Answer: 2  1  5  6  8  4  9  3  7 10
