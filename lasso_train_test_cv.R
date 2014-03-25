### Learning how to use ROCR
require(ROCR); require(knitr)
require(glmnet)

## Reading training data
data39lagraw <- read.csv(file='/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv',header=TRUE)
train <- data39lagraw[c(-1)]

head(train)

##### NOTE: data need to be standardized! or lasso will perform the worst
#####

# Standadize the training data 
train <- data.frame(scale(train[-61]))
is.matrix(train)
train$Y <- data39lagraw$X.1
head(train)

attach(train)




# Train lasso model --------------------------------------------------------

# glmnet() take a different input so we need different syntax here
# create the matrix x and then y
x <- model.matrix(Y~., train)
y <- train$Y

# create a grid to try different lambda
grid <- 10^seq(10, -2, length=100)
# Fit: alpha=0 is ridge, =1 is lasso. Pick 1 then.

## LASSO model:
lasso.mod <- glmnet(x,y,alpha=1, lambda=grid,family="binomial")
plot(lasso.mod)

lasso.pred = predict(lasso.mod, s=0.005, newx=x)


pred3 <- prediction(predictions=lasso.pred, labels=y)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, avg= "threshold", colorize=T, lwd= 3,main= "LASSO, AUC = 0.751")

# Get the AUC
auc3 <- performance(pred3,"auc")
# now converting S4 class to vector
auc3 <- unlist(slot(auc3, "y.values"))

