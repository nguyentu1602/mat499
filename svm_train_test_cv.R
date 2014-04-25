### Learning how to use ROCR
require(ROCR); require(knitr)
require(glmnet)
require(sampling)


## Reading training data
# data39lagraw <- read.csv(file='/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv',header=TRUE)
# testraw <- read.csv(file='/Users/cuongnguyen/Program/Git/mat499/fulldata39lag_2_to_40.csv',header=TRUE)

# Get rid of the first column, which is just index
train <- data39lagraw[c(-1)]
test <- testraw[-1]
train[1:10,]
# Standadize the training and testing data 
train$betweeness <- scale (as.numeric (train$betweeness))
test$betweeness <- scale (as.numeric (test$betweeness))

train <- data.frame(scale(train[-61]))
test <- data.frame(scale(test[-61]))
is.matrix(train)
train$Y <- data39lagraw$X.1
test$Y <- testraw$X.1
head(train)


# Train linear SVM model --------------------------------------------------

svmlinear.mod <- svm (Y~., data = train, kernel="linear", cost = 10, scale=FALSE)


