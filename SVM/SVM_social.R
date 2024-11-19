#Required Libraries
library(caTools)     #For sample.split function 
library(e1071)       #For svm function 
library(gmodels)     #For CrossTable function to create confusionmatrix

#Dataset is social.csv
social <- read.csv(file = "social.csv",stringsAsFactors = F)
summary(social)
str(social)

social<- social[,3:5]

social[-3]<- scale(social[-3])    #scaling the data except the target column

#Splitting the data into train and test
set.seed(143)
spl <- sample.split(social$Purchased,SplitRatio = 0.65)
train.soc <- subset(social,spl==T)
test.soc <- subset(social,spl==F)

#Training the SVM model
model <- svm(Purchased ~ .,
             train.soc,
             type = 'C-classification',
             kernel = 'linear')
model

#Prediction
pred<- predict(model,test.soc[-3])

#Confusion Matrix for the predictions
v<- CrossTable(test.soc[,3],pred)