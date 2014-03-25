### Learning how to use ROCR
require(ROCR); require(knitr)
require(glmnet)
require(sampling)
## Reading training data
#data39lagraw <- read.csv(file='/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv',header=TRUE)
#testraw <- read.csv(file='/Users/cuongnguyen/Program/Git/mat499/',header=TRUE)



train <- data39lagraw[c(-1)]
test <- testraw[-1]




head(train)
head(test)

##### NOTE: data need to be standardized! or lasso will perform the worst
#####

# Standadize the training and testing data 
train <- data.frame(scale(train[-61]))
test <- data.frame(scale(test[-61]))
is.matrix(train)
train$Y <- data39lagraw$X.1
test$Y <- testraw$X.1
head(train)

#attach(train)




# Train lasso model --------------------------------------------------------

# glmnet() take a different input so we need different syntax here
# create the matrix x and then y
x_train <- model.matrix(Y~., train)
y_train <- train$Y
x_test <- model.matrix(Y~.,test)
y_test <- test$Y

# create a grid to try different lambda
grid <- 10^seq(10, -2, length=100)
# Fit: alpha=0 is ridge, =1 is lasso. Pick 1 then.

## LASSO model:
lasso.mod <- glmnet(x_train,y_train,alpha=1, lambda=grid,family="binomial")
plot(lasso.mod)

# Predict on new data:
lasso.pred = predict(lasso.mod, s=0.005, newx=x_test)

## Cross validation to choose a good lambda for LASSO:


# Now to cross validation:
cv.out = cv.glmnet(x, y,alpha=1, nfolds=4)
?cv.glmnet
plot(cv.out)

bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx=x_test)




pred3 <- prediction(predictions=lasso.pred, labels=y_test)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, avg= "threshold", colorize=T, lwd= 3,main= "LASSO, AUC = 0.751")

# Get the AUC
auc3 <- performance(pred3,"auc")
# now converting S4 class to vector
auc3 <- unlist(slot(auc3, "y.values"))

