##################################
# Lecture 1: The Cate Package
##################################

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

set.seed(32343)
modelFit <- train(type~., data = training, method = "glm")
modelFit

modelFit$finalModel
predictions <- predict(modelFit, newdata = testing)
head(predictions,20)

confusionMatrix(predictions, testing$type)


##################################
# Lecture 2: Data Slicing
##################################

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

set.seed(32323)

folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Return just the test set
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds, length)
sum(sapply(folds, length))
folds[[1]][1:20]

# Resampling
set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Time slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[11]]
folds$test[[11]]


##################################
# Lecture 3: Training Options
##################################

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit <- train(type~., data = training, method = "glm")
modelFit

set.seed(1235)
modelFit2 <- train(type~., data = training, method = "glm")
modelFit2


##################################
# Lecture 4: Plotting Predictors
##################################

library(ISLR); library(ggplot2); library(caret)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age", "education", "jobclass")], y = training$wage, plot = "pairs")

qplot(age, wage, data = training)
qplot(age, wage, data = training, colour = jobclass)

qq <- qplot(age, wage, data = training, colour = education)
qq + geom_smooth(method = "lm", formula = y~x)

library(Hmisc)

cutWage <- cut2(training$wage, g = 3)
table(cutWage)

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom=c("boxplot"))
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol = 2)

t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1,1)

qplot(wage, colour = education, data = training, geom = "density")


##################################
# Lecture 5: PreProcessing
##################################

set.seed(32343)

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve, main = "", xlab = "average capital run length", breaks = 100)

mean(training$capitalAve)
median(training$capitalAve)
sd(training$capitalAve)
sd(training$capitalAve) / mean(training$capitalAve)

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

preObj <- preProcess(training[, -58], method = c("center", "scale"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

trainCapAveS <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32343)
modelFit <- train(type~., data = training, preProcess = c("center", "scale"), method = "glm")
modelFit

preObj <- preProcess(training[, -58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow = c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)


# Imputing Data

set.seed(13343)

# make some value NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[, -58], method = "knnImpute")
capAve <- predict(preObj, training[, -58])$capAve

# Standardize true value
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)

quantile(capAve - capAveTruth)

quantile((capAve - capAveTruth)[selectNA])

quantile((capAve - capAveTruth)[!selectNA])


##################################
# Lecture 6: Covariate Creation
##################################

# also called: Predictors, Features

library(kernlab); data(spam)
spam$capitalAveSq <- spam$capitalAve^2


library(ISLR); library(ggplot2); library(caret); data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

library(splines)
bsBasis <- bs(training$age, df = 3)
head(bsBasis)

lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata = training), col="red", pch=20, cex=2)

summary(lm1)

head(predict(bsBasis, age=testing$age))


##################################
# Lecture 7: Preprocessing with PCA
##################################

set.seed(32323)

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

names(training)[c(32, 34, 40)]

plot(spam[,34], spam[,32])

X <- 0.71 * training$num415 + 0.71*training$num857
Y <- 0.71 * training$num415 - 0.71*training$num857

plot(X,Y)

smallSpam <- spam[, c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

prComp$rotation

typeColor <- ((spam$type=="spam") * 1 + 1)
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[,1], prComp$x[,2], col = typeColor, xlab = "PC1", ylab = "PC2")
plot(prComp$x[,1], prComp$x[,2], col = spam$type, xlab = "PC1", ylab = "PC2")

prComp$rotation[,1:2]

preProc <- preProcess(log10(spam[,-58] + 1), method = "pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58] + 1))
plot(spamPC[,1], spamPC[,2], col=typeColor)

preProc <- preProcess(log10(training[,-58]+1), method = "pca", pcaComp = 2)
trainPC <- predict(preProc, log10(training[,-58] + 1))
trainPC <- cbind(trainPC, training$type)
names(trainPC) <- c("PC1", "PC2", "type")
head(trainPC)
modelFit <- train(type ~ ., method="glm", data=trainPC)

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))

# Alternatively (sets # of PCs)
modelFit <- train(training$type ~ ., method = "glm", preProcess = "pca", data = training)
confusionMatrix(testing$type, predict(modelFit, testPC))


##################################
# Lecture 8: Predicting with regression.
##################################

library(caret); data(faithful); set.seed(333)
inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]

head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

coef(lm1)[1] + coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

# Calculate RMSE on test
sqrt(sum((predict(lm1, newdata=testFaith) - testFaith$eruptions)^2))

#Prediction intervals
pred1 <- predict(lm1, newdata = testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type="l", col=c(1,2,2), lty = c(1,1,1), lwd=3)

modFit <- train(eruptions ~ waiting, data = trainFaith, method="lm")
summary(modFit$finalModel)


##################################
# Lecture 9: Predicting with regression, multiple covariates
##################################

# Example: Wage data
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage, select = -c(logwage))
summary(Wage)

# Get training/test sets
inTrain <- createDataPartition(y = Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Feature plot
featurePlot(x=training[, c("age","education","jobclass")], y = training$wage, plot="pairs")

# Plot age versus wage
qplot(age, wage, data = training)

# Plot age versus wage colour by jobclass
qplot(age, wage, colour = jobclass, data = training)

# Plot age versus wage colour by education
qplot(age, wage, colour = education, data = training)

# Fit a linear model
# ED_i = b_0 + b_1 age + b_2 I(Jobclass_i="Information") + \sum_{k=1}^4 \gamma_k I(education_i= level k) $$

modFit<- train(wage ~ age + jobclass + education, method = "lm", data = training)
finMod <- modFit$finalModel
print(modFit)

# Education levels: 1 = HS Grad, 2 = Some College, 3 = College Grad, 4 = Advanced Degree

# Diagnostics

plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

# Color by variables not used in the model
qplot(finMod$fitted, finMod$residuals, colour=race, data=training)

# Plot by index
plot(finMod$residuals, pch=19)

# Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage, pred, colour = year, data = testing)

# If you want to use all covariates
modFitAll <- train(wage ~ ., data = training, method = "lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data = testing)



