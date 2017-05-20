##############################################################
# QUIZ - Practical Machine Learning - WEEK 4
##############################################################

# QUESTION 1:
#----------------------------------------
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

# Random forests
# modfit.rf <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
modfit.rf <- train(y ~ ., data = vowel.train, method="rf")
modfit.gbm <- train(y ~ ., data = vowel.train, method="gbm")

pred.rf <- predict(modfit.rf, vowel.test)
pred.gbm <- predict(modfit.gbm, vowel.test)

t.rf <- table(pred.rf, vowel.test$y)
t.gbm <- table(pred.gbm, vowel.test$y)

# Both give the same result.
sum(diag(t.rf))/sum(t.rf)
confusionMatrix(pred.rf, vowel.test$y)$overall[1]

sum(diag(t.gbm))/sum(t.gbm)
confusionMatrix(pred.gbm, vowel.test$y)$overall[1]

predDF <- data.frame(pred.rf, pred.gbm, y = vowel.test$y)

# Accuracy among the test set samples where the two methods agree
id.agreed <- (pred.rf == pred.gbm)
confusionMatrix(pred.rf[id.agreed], vowel.test$y[id.agreed])$overall[1]
confusionMatrix(pred.gbm[id.agreed], vowel.test$y[id.agreed])$overall[1]

# Answer: 
# RF Accuracy = 0.6147, GBM Accuracy = 0.5368, Agreement Accuracy = 0.6656 (observed)
# RF Accuracy = 0.6082, GBM Accuracy = 0.5152, Agreement Accuracy = 0.6361 (quiz)

# QUESTION 2:
#----------------------------------------
library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

modfit.rf <- train(diagnosis ~ ., data = training, method="rf")
modfit.gbm <- train(diagnosis ~ ., data = training, method="gbm")
modfit.lda <- train(diagnosis ~ ., data = training, method="lda")

pred.rf <- predict(modfit.rf, testing)
pred.gbm <- predict(modfit.gbm, testing)
pred.lda <- predict(modfit.lda, testing)

com.data <- data.frame(pred.rf, pred.gbm, pred.lda, diagnosis=testing$diagnosis)

modfit.comb <- train(diagnosis ~ ., data = com.data, method="rf")
pred.comb <- predict(modfit.comb, testing)

confusionMatrix(pred.rf, testing$diagnosis)$overall[1]
confusionMatrix(pred.gbm, testing$diagnosis)$overall[1]
confusionMatrix(pred.lda, testing$diagnosis)$overall[1]

confusionMatrix(pred.comb, testing$diagnosis)$overall[1]

# Answer: Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.

# Stacked Accuracy: 0.80 is better than all three other methods - NOT THE ANSWER.


# QUESTION 3:
#----------------------------------------
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
pred.lasso <- train(CompressiveStrength ~ ., data = training, method="lasso")

plot.enet(pred.lasso$finalModel, xvar = "penalty", use.color=TRUE)

# Answer: Cement

# QUESTION 4:
#----------------------------------------
library(lubridate) # for year() function below
library(forecast)

dat <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr) # changes values to time-series format

# BATS model based on data before 2012
bats.model = bats(tstrain)

# build the forecast with the same range as the testing set (2012)
forecast.bats = forecast(bats.model, nrow(testing))

# plot the forecast
plot(forecast.bats)

# 95% prediction
forecast.bats.lower95 = forecast.bats$lower[,2]
forecast.bats.upper95 = forecast.bats$upper[,2]

# see how many of the testing visit counts do actually match
table ( 
  (testing$visitsTumblr > forecast.bats.lower95) & 
  (testing$visitsTumblr < forecast.bats.upper95))

226/(226+9)

# Answer: 0.9617

# QUESTION 5:
#----------------------------------------
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)

modfit.svm = svm(CompressiveStrength ~ ., data = training)

pred.svm = predict(modfit.svm, newdata = testing)

sqrt(mean((pred.svm - testing$CompressiveStrength)^2))

accuracy(f = pred.svm, x = testing$CompressiveStrength)

plot(pred.svm, testing$CompressiveStrength, pch=20, cex=2, col=testing$Superplasticizer,
     alpha=0.5, main="svm forecast vs actual values")

# Answer: 6.715
