#Libraries Required
library(e1071)      #for naive bayes
library(caTools)    #for splitting the data
library(gmodels)

#Analyzing the dataset
str(iris)
summary(iris)

#Splitting the data into training and testing
set.seed(123)
spl <- sample.split(iris$Species,SplitRatio = 0.7)
iris.train <- subset(iris,spl==T)
iris.test <- subset(iris,spl==F)

#Training the  model
model<- naiveBayes(as.matrix(iris.train),iris.train$Species)

#Predicting and confusion matrix
p<- predict(model,as.matrix(iris.test))

CrossTable(p,iris.test$Species,prop.chisq = F)