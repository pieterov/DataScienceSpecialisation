
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

# Prediction Model


# Random Forest - Random Forest package
# Model build
model.rfp <- randomForest(classe ~. , data = sub.train.set, importance = TRUE)
# Prediction
pred.rfp <- predict(model.rfp, sub.test.set)
# Test results on subTesting data set:
confusionMatrix(pred.rfp, sub.test.set$classe)$table
confusionMatrix(pred.rfp, sub.test.set$classe)$overall[1]

# Random Forest - Caret package
# Model build
model.rf <- train(classe ~. , data = sub.train.set, method = 'rf')
# Prediction
pred.rf <- predict(model.rf, sub.test.set)
# Test results on subTesting data set:
confusionMatrix(pred.rf, sub.test.set$classe)$table
confusionMatrix(pred.rf, sub.test.set$classe)$overall[1]

# Boosting with Trees - Caret package
# Model build
model.gbm <- train(classe ~. , data = sub.train.set, method = 'gbm')
# Prediction
pred.gbm <- predict(model.gbm, sub.test.set)
# Test results on subTesting data set:
confusionMatrix(pred.gbm, sub.test.set$classe)$table
confusionMatrix(pred.gbm, sub.test.set$classe)$overall[1]

# Linear Discriminant Analysis - Caret package
# Model build
model.lda <- train(classe ~. , data = sub.train.set, method = 'lda')
# Prediction
pred.lda <- predict(model.lda, sub.test.set)
# Test results on subTesting data set:
confusionMatrix(pred.lda, sub.test.set$classe)$table
confusionMatrix(pred.lda, sub.test.set$classe)$overall[1]

# Linear Discriminant Analysis - Caret package
# Combine the predicted data from the various models.
data.comb <- data.frame(pred.rfp, pred.rf, pred.gbm, pred.lda, classe = sub.test.set$classe)
# Model build.
model.comb <- train(classe ~ ., data = data.comb, method="rf")
# Prediction.
pred.comb <- predict(model.comb, sub.test.set)
# Test results on subTesting data set:
confusionMatrix(pred.comb, sub.test.set$classe)$table
confusionMatrix(pred.comb, sub.test.set$classe)$overall[1]

