#Required Libraries
library(caTools)    #for splitting the data ; for sample.split function
library(e1071)      #for svm function
 
#in-built Dataset mtcars
View(mtcars)
summary(mtcars)
str(mtcars)


#splitting the data into train and test
set.seed(92820)
s <- sample.split(mtcars$mpg ,SplitRatio = 0.7)
mtc.train <- subset(mtcars,s==T)
mtc.test <- subset(mtcars,s==F)

#Training the model
Model <- svm(as.factor(mpg) ~ . ,
             mtc.train,
             type = 'C-classification',
             kernel = 'linear')
Model

#Prediction
predict(Model,mtc.test)