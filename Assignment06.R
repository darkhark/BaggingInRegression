setwd("C:/Users/joshu/Documents/STA5703/BaggingInRegression")
table = read.csv(file = "data/ASS06_Data.csv")
print("Data Read Into Variable")

# Question 1
totalRows = 1460
predictions1 = data.frame()

for(i in 1:20){
  print(i)
  column = sprintf("%d", i)
  temp = vector()
  sample <- sample(1:nrow(table), size=totalRows, replace =T)
  model = lm(SalePrice ~., data = table[sample, ])
  for(j in 1:length(sample)){
    predictions1[j, column] = predict(model, table[j, ])
  }
}

estimator1 = data.frame()
for(i in 1:totalRows){
  sumOfPredictions = 0
  for(j in 1:20){
    sumOfPredictions = sumOfPredictions + predictions1[i, j]
  }
  estimator1[i, 1] = sumOfPredictions / 20
}

mse1 = 0
for(i in 1:20){
  for(j in 1:totalRows){
    mse1 = mse1 + ((predictions1[j, i] - estimator1[j, ])^2)/totalRows
  }
}
mse1 = mse1/20
print("Mean Squared Error: ")
print(mse1)
boxplot(predictions1)

#Question 2
library(party)
predictions2 = data.frame()

for(i in 1:20){
  print(i)
  column = sprintf("%d", i)
  temp = vector()
  sample = sample(1:nrow(table), size=totalRows, replace=T)
  model = ctree(SalePrice ~., data = table[sample, ])
  for(j in 1:length(sample)){
    predictions2[j, column] = predict(model, table[j, ])
  }
}

estimator2 = data.frame()
for(i in 1:totalRows){
  sumOfPredictions2 = 0
  for(j in 1:20){
    sumOfPredictions2 = sumOfPredictions2 + predictions2[i, j]
  }
  estimator2[i, 1] = sumOfPredictions2 / 20
}

mse2 = 0
for(i in 1:20){
  for(j in 1:totalRows){
    mse2 = mse2 + ((predictions2[j, i] - estimator2[j, ])^2)/totalRows
  }
}
mse2 = mse2/20
print("Mean Squared Error:")
print(mse2)
boxplot(predictions2)