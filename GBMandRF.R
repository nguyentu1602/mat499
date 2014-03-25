# Testing randomforest and GBM with my data:
require(gbm)
<<<<<<< Updated upstream
require(foreach)
require(parallel)
require(ROCR)
detectCores()
full = read.csv("/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv", header= TRUE)
=======
require(parallel)

detectCores()
full = read.csv("~/csc207/Git/mat499/fullData39Lag.csv", header= TRUE)
>>>>>>> Stashed changes
N = length(full[,1])
dim(full)

#### Calculate AUC for first GBM model
predict1 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predict.csv", header= TRUE)
predict1 = predict1[,-1]

# Step 1: Create an object of type prediction using ROCR
pred1 <- prediction(predictions=predict1, labels=full$X.1)

# Step 2: Create an object of type performance using ROCR
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, avg= "threshold", colorize=T, lwd= 3,main= "Logit model, AUC=0.756")

# Step 3: Get the AUC
auc1 <- performance(pred1,"auc")
# now converting S4 class to vector
auc1 <- unlist(slot(auc1, "y.values"))





#### Calculate AUC for second GBM model
predict2 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel2.csv", header= TRUE)
predict2 = predict2[,-1]

# Step 1: Create an object of type prediction using ROCR
pred2 <- prediction(predictions=predict2, labels=full$X.1)

# Step 2: Create an object of type performance using ROCR
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, avg= "threshold", colorize=T, lwd= 3,main= "Logit model, AUC=0.756")

# Step 3: Get the AUC
auc2 <- performance(pred2,"auc")
# now converting S4 class to vector
auc2 <- unlist(slot(auc2, "y.values"))




#### Calculate AUC for third GBM model
predict3 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel3.csv", header= TRUE)
predict3 = predict3[,-1]

# Step 1: Create an object of type prediction using ROCR
pred3 <- prediction(predictions=predict3, labels=full$X.1)

# Step 2: Create an object of type performance using ROCR
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, avg= "threshold", colorize=T, lwd= 3,main= "Logit model, AUC=0.756")

# Step 3: Get the AUC
auc3 <- performance(pred3,"auc")
# now converting S4 class to vector
auc3 <- unlist(slot(auc3, "y.values"))


#### Calculate AUC for fourth-to-eigth GBM model
predict4 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel4.csv", header= TRUE)[,-1]
predict5 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel5.csv", header= TRUE)[,-1]
predict6 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel6.csv", header= TRUE)[,-1]
predict7 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel7.csv", header= TRUE)[,-1]
predict8 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel8.csv", header= TRUE)[,-1]
predict9 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel9.csv", header= TRUE)[,-1]
predict10 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel10.csv", header= TRUE)[,-1]
predict11 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel11.csv", header= TRUE)[,-1]
predict12 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel12.csv", header= TRUE)[,-1]
predict13 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel13.csv", header= TRUE)[,-1]
predict14 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel14.csv", header= TRUE)[,-1]
predict15 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel15.csv", header= TRUE)[,-1]
predict16 = read.csv("/Users/cuongnguyen/Program/Git/mat499/predictGBMmodel16.csv", header= TRUE)[,-1]




# Step 1: Create an object of type prediction using ROCR
pred4 <- prediction(predictions=predict4, labels=full$X.1)
pred5 <- prediction(predictions=predict5, labels=full$X.1)
pred6 <- prediction(predictions=predict6, labels=full$X.1)
pred7 <- prediction(predictions=predict7, labels=full$X.1)
pred8 <- prediction(predictions=predict8, labels=full$X.1)
pred9 <- prediction(predictions=predict9, labels=full$X.1)
pred10 <- prediction(predictions=predict10, labels=full$X.1)
pred11 <- prediction(predictions=predict11, labels=full$X.1)
pred12 <- prediction(predictions=predict12, labels=full$X.1)
pred13 <- prediction(predictions=predict13, labels=full$X.1)
pred14 <- prediction(predictions=predict14, labels=full$X.1)
pred15 <- prediction(predictions=predict15, labels=full$X.1)
pred16 <- prediction(predictions=predict16, labels=full$X.1)



# Step 2: Create an object of type performance using ROCR
perf4 <- performance(pred4, "tpr", "fpr")
perf5 <- performance(pred5, "tpr", "fpr")
perf6 <- performance(pred6, "tpr", "fpr")
perf7 <- performance(pred7, "tpr", "fpr")
perf8 <- performance(pred8, "tpr", "fpr")
perf9 <- performance(pred9, "tpr", "fpr")
perf10 <- performance(pred10, "tpr", "fpr")
perf11 <- performance(pred11, "tpr", "fpr")
perf12 <- performance(pred12, "tpr", "fpr")
perf13 <- performance(pred13, "tpr", "fpr")
perf14 <- performance(pred14, "tpr", "fpr")
perf15 <- performance(pred15, "tpr", "fpr")
perf16 <- performance(pred16, "tpr", "fpr")


# Step 3: Get the AUC
auc4 <- performance(pred4,"auc")
auc5 <- performance(pred5,"auc")
auc6 <- performance(pred6,"auc")
auc7 <- performance(pred7,"auc")
auc8 <- performance(pred8,"auc")
auc9 <- performance(pred9,"auc")
auc10 <- performance(pred10,"auc")
auc11 <- performance(pred11,"auc")
auc12 <- performance(pred12,"auc")
auc13 <- performance(pred13,"auc")
auc14 <- performance(pred14,"auc")
auc15 <- performance(pred15,"auc")
auc16 <- performance(pred16,"auc")

# now converting S4 class to vector
auc4 <- unlist(slot(auc4, "y.values"))
auc5 <- unlist(slot(auc5, "y.values"))
auc6 <- unlist(slot(auc6, "y.values"))
auc7 <- unlist(slot(auc7, "y.values"))
auc8 <- unlist(slot(auc8, "y.values"))
auc9 <- unlist(slot(auc9, "y.values"))
auc10 <- unlist(slot(auc10, "y.values"))
auc11 <- unlist(slot(auc11, "y.values"))
auc12 <- unlist(slot(auc12, "y.values"))
auc13 <- unlist(slot(auc13, "y.values"))
auc14 <- unlist(slot(auc14, "y.values"))
auc15 <- unlist(slot(auc15, "y.values"))
auc16 <- unlist(slot(auc16, "y.values"))





# First try RF and GBM with the whole dataset
# Don't need interaction terms!
pr = 0 # Prediction accuracy for training data
tr = 0 # Prediction accuracy for testing data
n.models = 2 # Number of models to try
<<<<<<< Updated upstream
GBM.model= vector()

head(full)

foreach(i in 1:n.models) %dopar% {
  GBM.model[i] = gbm.fit(x= full[,3:61], y = full[,62], distribution="adaboost", 
                      n.trees= 1000, shrinkage=0.05, interaction.depth=5, n.minobsinnode=50)
  
  # Training set Predictions
  #pr1 = predict(object=GBM.model,newdata=full[,2:61], n.trees=1000)
  # Add to the old one:
  #pr = pr+pr1
  
=======

head(full)

GBM.model = gbm(X.1~. -X -X1, data=full, distribution="gaussian", 
                n.trees= 5000, shrinkage=0.05, interaction.depth=4, 
                n.minobsinnode=30, cv.folds = 4,n.cores= 4)

GBM.model2 = gbm(X.1~. -X -X1, data=full, distribution="adaboost", 
                n.trees= 5000, shrinkage=0.01, interaction.depth=4, 
                n.minobsinnode=20, cv.folds = 4,n.cores= 4)

GBM.model3 = gbm(X.1~. -X -X1, data=full, distribution="gaussian", 
                 n.trees= 3000, shrinkage=0.01, interaction.depth=5, 
                 n.minobsinnode=40, cv.folds = 4,n.cores= 4)

GBM.model4 = gbm(X.1~. -X -X1, data=full, distribution="gaussian", 
                 n.trees= 3000, shrinkage=0.01, interaction.depth=4, 
                 n.minobsinnode=30, cv.folds = 4,n.cores= 4)

GBM.model5 = gbm(X.1~.-X-X1-X2-X3-X4, data=full, distribution="gaussian", 
                 n.trees= 8000, shrinkage=0.02, interaction.depth=4, 
                 n.minobsinnode=30, cv.folds = 8,n.cores= 4)

GBM.model6 = gbm(X.1~.-X-X1-X2-X3-X4, data=full, distribution="gaussian", 
                 n.trees= 8000, shrinkage=0.02, interaction.depth=4, 
                 n.minobsinnode=30, cv.folds = 8,class.stratify.cv =TRUE,n.cores= 4)

GBM.model7 = gbm(X.1~.-X-X1-X2-X3-X4, data=full, distribution="gaussian", 
                 n.trees= 10000, shrinkage=0.01, interaction.depth=5, 
                 n.minobsinnode=30, n.cores= 4)

GBM.model8 = gbm(X.1~.-X-X1-X2-X3-X4, data=full, distribution="gaussian", 
                 n.trees= 10000, shrinkage=0.001, interaction.depth=5, 
                 n.minobsinnode=30, n.cores= 4)

GBM.model9 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10, data=full, distribution="gaussian", 
                 n.trees= 20000, shrinkage=0.01, interaction.depth=4, 
                 n.minobsinnode=30, n.cores= 4)

GBM.model10 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10, data=full, distribution="gaussian", 
                 n.trees= 20000, shrinkage=0.01, interaction.depth=5, 
                 n.minobsinnode=30, n.cores= 4)

GBM.model11 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10, data=full, distribution="gaussian", 
                  n.trees= 40000, shrinkage=0.005, interaction.depth=5, 
                  n.minobsinnode=40, n.cores= 4)

GBM.model12 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10, data=full, distribution="gaussian", 
                  n.trees= 40000, shrinkage=0.005, interaction.depth=6, 
                  n.minobsinnode=30, n.cores= 4)

GBM.model13 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10-X12-X13-X14-X15, data=full, distribution="gaussian", 
                  n.trees= 10000, shrinkage=0.02, interaction.depth=4, 
                  n.minobsinnode=50, n.cores= 4)

GBM.model14 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10-X12-X13-X14-X15, data=full, distribution="gaussian", 
                  n.trees= 10000, shrinkage=0.02, interaction.depth=3, 
                  n.minobsinnode=60, n.cores= 4)

GBM.model15 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10-X11-X12-X13-X14-X15, data=full, distribution="gaussian", 
                  n.trees= 5000, shrinkage=0.05, interaction.depth=6, 
                  n.minobsinnode=40, n.cores= 4)

GBM.model16 = gbm(X.1~.-X-X1-X2-X3-X4-X5-X6-X7-X8-X9-X10-X11-X12-X13-X14-X15, data=full, distribution="gaussian", 
                  n.trees= 5000, shrinkage=0.05, interaction.depth=7, 
                  n.minobsinnode=50, n.cores= 4)
#New models:

GBM.model17 = gbm(X.1~.-X-X1, data=full, distribution="gaussian", 
                  n.trees= 4000, shrinkage=0.01, interaction.depth=2, 
                  n.minobsinnode=30, n.cores= 4)

GBM.model18 = gbm(X.1~.-X-X1-X2-X3, data=full, distribution="gaussian", 
                  n.trees= 8000, shrinkage=0.005, interaction.depth=2, 
                  n.minobsinnode=30, n.cores= 4)

GBM.model19 = gbm(X.1~.-X-X1-X2-X3-X4-X5, data=full, distribution="gaussian", 
                  n.trees= 10000, shrinkage=0.01, interaction.depth=3, 
                  n.minobsinnode=30, n.cores= 4)

GBM.model20 = gbm(X.1~.-X-X1-X2-X3-X4-X5, data=full, distribution="gaussian", 
                  n.trees= 8000, shrinkage=0.01, interaction.depth=3, 
                  n.minobsinnode=20, n.cores= 4)

GBM.model21 = gbm(X.1~.-X-X1-X2-X3, data=full, distribution="gaussian", 
                  n.trees= 8000, shrinkage=0.01, interaction.depth=2, 
                  n.minobsinnode=20, n.cores= 4)

# GBM.model22 = gbm(X.1~.-X-X1, data=full, distribution="gaussian", 
#                   n.trees= 10000, shrinkage=0.005, interaction.depth=2, 
#                   n.minobsinnode=15, n.cores= 4)

GBM.model22 = gbm(X.1~.-X-X1, data=full, distribution="gaussian", 
                  n.trees= 4000, shrinkage=0.01, interaction.depth=3, 
                  n.minobsinnode=20, n.cores= 4)

GBM.model23 = gbm(X.1~.-X-X1, data=full, distribution="gaussian", 
                  n.trees= 5000, shrinkage=0.01, interaction.depth=4, 
                  n.minobsinnode=20, n.cores= 4)

summary(GBM.model15)
summary(GBM.model12)
summary(GBM.model11)



summary(GBM.model16)



summary(GBM.model8)


summary(GBM.model2)
GBM.model2




for(i in 1:n.models) {
  GBM.model = gbm(X.1~. -X -X1, data=full, distribution="adaboost", 
                      n.trees= 1000, shrinkage=0.05, interaction.depth=5, n.minobsinnode=30, n.cores= 4)
  
  # Training set Predictions
  # pr1 = predict(object=GBM.model,newdata=full[,3:61], n.trees=1000)
  # Add to the old one:
  #pr = pr+pr1
>>>>>>> Stashed changes
}
pr1 = predict(object=GBM.model,newdata=full[,3:61], n.trees=5000)
write.csv(pr1, file = "~/csc207/Git/mat499/predict.csv")
pr2 = predict(object=GBM.model2,newdata=full[,3:61], n.trees=5000)
write.csv(pr2, file = "~/csc207/Git/mat499/predictGBMmodel2.csv")
pr3 = predict(object=GBM.model3,newdata=full[,3:61], n.trees=3000)
write.csv(pr3, file = "~/csc207/Git/mat499/predictGBMmodel3.csv")
pr4 = predict(object=GBM.model4,newdata=full[,3:61], n.trees=3000)
write.csv(pr4, file = "~/csc207/Git/mat499/predictGBMmodel4.csv")
pr5 = predict(object=GBM.model5,newdata=full[,6:61], n.trees=8000)
write.csv(pr5, file = "~/csc207/Git/mat499/predictGBMmodel5.csv")
pr6 = predict(object=GBM.model6,newdata=full[,6:61], n.trees=8000)
write.csv(pr6, file = "~/csc207/Git/mat499/predictGBMmodel6.csv")
pr7 = predict(object=GBM.model7,newdata=full[,6:61], n.trees=10000)
write.csv(pr7, file = "~/csc207/Git/mat499/predictGBMmodel7.csv")
pr8 = predict(object=GBM.model8,newdata=full[,6:61], n.trees=10000)
write.csv(pr8, file = "~/csc207/Git/mat499/predictGBMmodel8.csv")
pr9 = predict(object=GBM.model9,newdata=full[,12:61], n.trees=20000)
write.csv(pr9, file = "~/csc207/Git/mat499/predictGBMmodel9.csv")
pr10 = predict(object=GBM.model10,newdata=full[,12:61], n.trees=20000)
write.csv(pr10, file = "~/csc207/Git/mat499/predictGBMmodel10.csv")
pr11 = predict(object=GBM.model11,newdata=full[,12:61], n.trees=40000)
write.csv(pr11, file = "~/csc207/Git/mat499/predictGBMmodel11.csv")
pr12 = predict(object=GBM.model12,newdata=full[,12:61], n.trees=40000)
write.csv(pr12, file = "~/csc207/Git/mat499/predictGBMmodel12.csv")
pr13 = predict(object=GBM.model13,newdata=full[,c(12,17:61)], n.trees=10000)
write.csv(pr13, file = "~/csc207/Git/mat499/predictGBMmodel13.csv")
pr14 = predict(object=GBM.model14,newdata=full[,c(12,17:61)], n.trees=10000)
write.csv(pr14, file = "~/csc207/Git/mat499/predictGBMmodel14.csv")
pr15 = predict(object=GBM.model15,newdata=full[,17:61], n.trees=5000)
write.csv(pr15, file = "~/csc207/Git/mat499/predictGBMmodel15.csv")
pr16 = predict(object=GBM.model16,newdata=full[,17:61], n.trees=5000)
write.csv(pr16, file = "~/csc207/Git/mat499/predictGBMmodel16.csv")



save(list=ls(pattern="GBM"), file="GBMmodels.R")

getwd()



head(full[,c(12,17:61)])




length(pr1)
length(full[,62])



# Average the prediction:
<<<<<<< Updated upstream
pr = pr/n.models


pr
=======
head(pr1)
install.packages("verification")
install.packages("gplots")
require(gplots)
require(verification)
install.packages("ROCR")
>>>>>>> Stashed changes
