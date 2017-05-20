##################################
# Lecture 1: Predicting with trees 
##################################

data(iris); library(ggplot2)
names(iris)
table(iris$Species)

#Create training and test sets
library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Iris petal widths/sepal width
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

# Iris petal widths/sepal width
modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

# Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# Prettier plots
# library(rattle) --> installation not succesful
# fancyRpartPlot(modFit$finalModel)

# Predicting new values
predict(modFit,newdata=testing)


##################################
# Lecture 2: Bagging
##################################

# Ozone data
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)
# http://en.wikipedia.org/wiki/Bootstrap_aggregating

# Bagged loess
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1],replace=T)
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
        loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
        ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

# Bagged loess
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

# More bagging in caret
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
# http://www.inside-r.org/packages/cran/caret/docs/nbBag

# Example of custom bagging (continued)
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")

# Parts of bagging
ctreeBag$fit

# Parts of bagging
ctreeBag$pred

# Parts of bagging
ctreeBag$aggregate


##################################
# Lecture 3: Random Forests
##################################

# Iris data
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# Random forests
library(caret)
modFit <- train(Species~ ., data=training, method="rf", prox=TRUE)
modFit

# Getting a single tree
getTree(modFit$finalModel,k=2)

# Class "centers"
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

# Predicting new values
pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species
table(pred,testing$Species)

# Predicting new values
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")


##################################
# Lecture 4: Boosting 
##################################

library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

# Fit the model
modFit <- train(wage~., method="gbm", data=training, verbose=FALSE)
print(modFit)

# Plot the results
qplot(predict(modFit,testing),wage,data=testing)


##################################
# Lecture 4: Model Based Prediction
##################################

data(iris); library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Build predictions
modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)

# Comparison of results
equalPredictions = (plda==pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)