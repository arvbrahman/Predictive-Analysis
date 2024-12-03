# you are data scientist work on project to analyze the growth of chicks
#you are given "chickweight" inbuilt dataset,which record weight of chicks at different times points
#across various diet ,goal is to predict weight of the chicks using available featuers ,your task to build a svm model to predict weight of chicks based on time and diet

#Library
library(caTools)    #for splitting the data
library(e1071)      #contains svm function

#working on chickweight in-built dataset
data<- ChickWeight

summary(data)
hist(data$weight)
str(data)

#Splitting into train and test
set.seed(134)
split<- sample.split(data$weight,SplitRatio = 0.7)
train_x <- subset(data,split==T)
test_y <- subset(data,split==F)

#Training the model
Model <- svm(weight ~ . -Chick ,
             train_x,
             type = 'nu-regression',
             kernel = 'linear')
Model

#Prediction
predict(Model,test_y)