#Loading the required libraries
library(ISLR)
library(rpart)
library(caTools)
library(rpart.plot)

#Carseats dataset from ISLR package
Cs<- Carseats

#Splitting the data into train and test
set.seed(123)
spl<- sample.split(Cs$ShelveLoc,SplitRatio = 0.70)
Cs.train<- subset(Cs,spl==T)
Cs.test<- subset(Cs, spl==F)

#Separating the target variable
Target<- ShelveLoc ~ . 

#Training the model
Model<- rpart(Target,Cs.train,method = "class")

#Plotting the decision Tree
rpart.plot(Model)

plot(Model)
text(Model,pretty = 0)

#Predicting the values for test data 
predicted<- predict(Model,Cs.test,type = "class")

#
table(predicted,Cs.test$ShelveLoc)

#
set.seed(234)
