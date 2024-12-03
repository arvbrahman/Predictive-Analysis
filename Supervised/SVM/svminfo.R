#Library
library(caTools)    #for splitting the data
library(e1071)      #contains both svm and naiveBayes

#working on iris in-built dataset
i<- iris

summary(i)
hist(i$Sepal.Width)
str(i)

#Splitting into train and test
set.seed(134)
split<- sample.split(i$Species,SplitRatio = 0.75)
i.train <- subset(i,split==T)
i.test<- subset(i,split==F)

#Training the model
Model <- svm(Species ~ . ,
             i.train,
             type = 'C-classification',
             kernel = 'linear')
Model

#Prediction
predict(Model,i.test)