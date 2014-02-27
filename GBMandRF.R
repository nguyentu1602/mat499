# Testing randomforest and GBM with my data:
require(gbm)
full = read.csv("/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv", header= TRUE)
N = length(full[,1])
dim(full)

# First try RF and GBM with the whole dataset
# Don't need interaction terms!
pr = 0 # Prediction accuracy for training data
tr = 0 # Prediction accuracy for testing data
n.models = 5 # Number of models to try

head(full)

for(i in 1:n.models) {
  GBM.model = gbm.fit(x= full[,2:61], y = full[,62], distribution="adaboost", 
                      n.trees= 1000, shrinkage=0.01, interaction.depth=10, n.minobsinnode=5)
  
  # Training set Predictions
  pr1 = predict(object=GBM.model,newdata=full[,2:61], n.trees=1000)
  # Add to the old one:
  pr = pr+pr1
  
}

# Average the prediction:
pr = pr/n.models