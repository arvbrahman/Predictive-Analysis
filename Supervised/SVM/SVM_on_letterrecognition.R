#Required Libraries 
library(kernlab)  
library(caTools)     #For splitting the data

#Dataset being used is the letterrecognition data
letter_data <- read.csv(file = "letterrecognition.csv",stringsAsFactors = T)

str(letter_data)
summary(letter_data)

#splitting the data
set.seed(123)
spl <- sample.split(letter_data$letter,SplitRatio = 0.8)
Train.letter <- subset(letter_data,spl==T)
Test.letter <- subset(letter_data,spl==F)

#Training the model with vanilladot
Model<- ksvm(letter ~ .,
             Train.letter,
             kernel = "vanilladot")
Model

#Predicting on the testing data
Predictions<- predict(Model,Test.letter)

table(Predictions, Test.letter$letter)
agreement <- Predictions==Test.letter$letter
table(agreement)
prop.table(table(agreement))


#Training with rbfdot kernel
Model.rbf<- ksvm(letter ~ .,
             Train.letter,
             kernel = "rbfdot")    #Radial Basic Function, when linear cannot be used on the data
Model.rbf

#Predicting 
predict.rbf <- predict(Model.rbf,Test.letter)

table(predict.rbf,Test.letter$letter)
agreement.rbf <- predict.rbf==Test.letter$letter
table(agreement.rbf)
prop.table(table(agreement.rbf))
