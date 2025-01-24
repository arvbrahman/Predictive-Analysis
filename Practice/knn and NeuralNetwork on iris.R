#Required Libraries
library(class)
library(neuralnet)
library(caTools)

#Loading dataset
iris.data <- iris

#Normalization
norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}
iris.norm <-as.data.frame(lapply(iris.data[1:4],norm))     #Standardizing the data

#Splitting the data into train and test
spl <- sample.split(iris$Species,SplitRatio = 0.7)
train.data <- subset(iris.norm,spl == T)
test.data <- subset(iris.norm,spl == F)