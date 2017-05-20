
# Initialize libraries.
library(caret); library(randomForest)

# Getting the data
# Some of the data fields contain "#DIV/0!" or are blank (""). These fields will be filled with "NA".

train.path <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.path <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train.set <- read.csv(train.path, head=TRUE, sep=',', na.strings = c("NA", "#DIV/0!", ""))
test.set <- read.csv(test.path, head=TRUE, sep=',', na.strings = c("NA", "#DIV/0!", ""))

dim(train.set); dim(test.set)
head(train.set, 3); head(test.set, 3)

# We see that some variables are not relevant for the analysis we are asked to do. These - non-relevant - variables are X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, and num_window. These are the first 7 columns.
train.set <- train.set[ , -c(1:7)]
test.set <- test.set[ , -c(1:7)]

dim(train.set); dim(test.set)

# We remove columns that contain NA.
train.set <- train.set[,colSums(is.na(train.set)) == 0]
test.set <- test.set[,colSums(is.na(test.set)) == 0]

# Just to ensure there is no NA left in the data, we sum if there are any,
sum(is.na(train.set))
sum(is.na(test.set))

# We check dimension/columns of the remaining data.
dim(train.set); dim(test.set)

# Cross-validation
# The training set is split in to 2 subsets in order to conduct cross-validation. The sub.training set will get 75% of the data, while the sub.test set will get the remaining 25% of the data. Assignment will be done using random sampling without replacement.
sub.set <- createDataPartition(y = train.set$classe, p=0.75, list=FALSE)
sub.train.set <- train.set[ sub.set, ] 
sub.test.set <- train.set[ -sub.set, ]

dim(sub.train.set); dim(sub.test.set)
head(sub.train.set, 3); head(sub.train.set, 3)

# Let's review the variable "classe". The three tables show how the levels are distributed.
table(train.set$classe)
table(sub.train.set$classe)
table(sub.test.set$classe)

# Prediction Model - Random Forest

# Model Training
# We use the randomForest function from the same named 'randomForest' package.
model.rf <- randomForest(classe ~. , data = sub.train.set, importance = TRUE)

# Model Validation
# We will review how the model performs on the training set itself and the cross validation set.

# Training set accuracy.
pred.rf.sub.train <- predict(model.rf, sub.train.set)
# Test results on sub.train data set:
confusionMatrix(pred.rf.sub.train, sub.train.set$classe)$table
confusionMatrix(pred.rf.sub.train, sub.train.set$classe)$overall

# As expected, the model performs very well against the training set. 
# Next, we will cross validate the model performance against the subset that was not used in the model training.

# Cross validation set accuracy.
pred.rf.sub.test <- predict(model.rf, sub.test.set)
# Test results on sub.test data set:
confusionMatrix(pred.rf.sub.test, sub.test.set$classe)$table
confusionMatrix(pred.rf.sub.test, sub.test.set$classe)$overall

# The cross validation accuracy is 99.7%. This makes the out-of-sample error equal to 0.3%.

# Test set prediction.
# The model predicts the following classe levels for the test set.
pred.rf.test <- predict(model.rf, test.set)
pred.rf.test

