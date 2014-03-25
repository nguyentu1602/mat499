### Learning how to use ROCR
### Comparing ROCR of logit, ridge and LASSO
require(ROCR); require(knitr)
data39lagraw <- read.csv(file='/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv',header=TRUE)
train <- data39lagraw[c(-1)]

head(train)

##### NOTE: data need to be standardized! or lasso will perform the worst
#####

# Standadize the data 
train <- data.frame(scale(train[-61]))
is.matrix(train)
train$Y <- data39lagraw$X.1
head(train)

#logit1 <- glm(Y ~ ., data=data39lag, family=binomial)
logit2 <- glm(Y ~ . + fromInDegree*fromSecondLevelInNodeInDeg + toInDegree*toSecondLevelOutNodeOutDeg   
              -X1 - X2 - X3 -X4 - fromCloseness - toCloseness, data=data39lag, family=binomial)
attach(train)
predict.plot(Y ~ fromInDegree | fromOutDegree, data=data39lag )


summary(logit2)

# Step 0: vectors of output and labels
# Vector of prediction 
glm.prob <- predict(logit2, type = "response")

# Vector of lable is just Y


# Step 1: Create an object of type prediction using ROCR
pred1 <- prediction(predictions=glm.prob, labels=data39lag$Y)

# Step 2: Create an object of type performance using ROCR
perf1 <- performance(pred1, "tpr", "fpr")

plot(perf1, avg= "threshold", colorize=T, lwd= 3,main= "Logit model, AUC=0.756")

# Step 3: Get the AUC
auc1 <- performance(pred1,"auc")
# now converting S4 class to vector
auc1 <- unlist(slot(auc1, "y.values"))

data39lag$weight

qplot(fromInDegree, toInDegree, col = Y, data=data39lag, xlim= c(-1,4), ylim = c(-1,0.5))

qplot(fromInDegree, fromOutDegree, col = Y, data=data39lag, xlim= c(-0.5,1), ylim = c(-0.5,0.5))
qplot(fromInDegree*weight, fromOutDegree*weight, col = Y, data=data39lag, xlim= c(-1,3), ylim = c(-1,3))


qplot(toInDegree, fromInDegree, col = Y, data=data39lag, xlim= c(-1,5), ylim = c(-1,5))

qplot(percentOf.1., weight, col = Y, data=data39lag)
qplot(countOfSwitch, weight, col = Y, data=data39lag)
qplot(countOfSwitch*weight, fromInDegree, col = Y, data=data39lag)
qplot(countOfSwitch, fromInDegree, col = Y, data=data39lag)
qplot(countOfSwitch, fromInDegree, col = Y, data=data39lag)
qplot(fromInDegree*fromSecondLevelInNodeInDeg, fromInDegree, col = Y, data=data39lag)
qplot(fromInDegree, fromSecondLevelInNodeInDeg, col = Y, data=data39lag,xlim= c(-1,5), ylim = c(-1,5))

pairs(~ Y +percentOf.1.+ countOfSwitch + fromSecondLevelInNodeInDeg + fromSecondLevelOutNodeOutDeg
      ,data=data39lag)


pairs(data39lag[50:61])

qplot(fromInDegree, toInDegree, col = Y, data=data39lag)
qplot(fromInDegree, toInDegree, col = Y, data=data39lag)


?pairs




# Ridge regression --------------------------------------------------------
#install.packages("glmnet"); 
require(glmnet)

# glmnet() take a different input so we need different syntax here
# create the matrix x and then y
x <- model.matrix(Y~., train)
y <- train$Y

# create a grid to try different lambda
grid <- 10^seq(10, -2, length=100)
# Fit ridge:alpha=0 is ridge, =1 is lasso
ridge.mod <- glmnet(x,y,alpha=0, lambda=grid, thresh=1e-12)

dim(coef(ridge.mod))
ridge.mod$lambda[50]

# create vector of prediction:
ridge.pred = predict(ridge.mod, s=212, newx=x)
par(mfrow=c(2,3))

dim(ridge.pred)
length(glm.prob)

# now to the fun part of AUC! 

pred2 <- prediction(predictions=ridge.pred, labels=y)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, avg= "threshold", colorize=T, lwd= 3,main= "Ridge, Lambda=4, AUC=0.731")

# Get the AUC
auc2 <- performance(pred2,"auc")
# now converting S4 class to vector
auc2 <- unlist(slot(auc2, "y.values"))


# Now try LASSO
lasso.mod <- glmnet(x,y,alpha=1, lambda=grid)
plot(lasso.mod)
lasso.pred = predict(lasso.mod, s=0.005, newx=x)
lasso.pred

pred3 <- prediction(predictions=lasso.pred, labels=y)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, avg= "threshold", colorize=T, lwd= 3,main= "LASSO, AUC = 0.745")

# Get the AUC
auc3 <- performance(pred3,"auc")
# now converting S4 class to vector
auc3 <- unlist(slot(auc3, "y.values"))

###### Now randomly create data:
norm.pred = rnorm(n=length(y))
binom.pred = rbinom(size=1,n=length(y),prob=0.99)

pred4 <- prediction(predictions=binom.pred, labels=y)
perf4 <- performance(pred4, "tpr", "fpr")
auc4 <- performance(pred4,"auc")
auc4 <- unlist(slot(auc4, "y.values"))

plot(perf4, avg= "threshold", colorize=T, lwd= 3,main= "Random guess alltrue\n AUC=0.501")
