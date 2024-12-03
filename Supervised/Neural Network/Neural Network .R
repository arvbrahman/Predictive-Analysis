#Libraries Required
library(neuralnet)

#Dataset
Cd <- read.csv(file = "Concrete_Data.csv",stringsAsFactors = F)

#Analyzing
hist(Cd$strength)
summary(Cd)
str(Cd)

#Normalizing the data 
normalize <- function(x){return((x-min(x))/(max(x)-min((x))))}
Cd.norm <- data.frame(lapply(Cd,normalize))
hist(Cd.norm$strength)

#Splitting the data into train and test
Cd.train <- Cd.norm[1:774,]
Cd.test <- Cd.norm[775:1030,]

#Model training
Cd.model <- neuralnet(strength ~ . ,Cd.train,hidden = 7)     #Single Hidden Node
plot(Cd.model)   #plotting

#Testing 
compute(Cd.model,Cd.test[,1:8])
predict(Cd.model,Cd.test[,1:8])