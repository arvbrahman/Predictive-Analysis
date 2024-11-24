#
library(class)
library(neuralnet)
library(caTools)

iris.data <- iris

norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}
iris.norm <-as.data.frame(lapply(iris.data[1:4],norm))

#
spl <- sample.split(iris$Species,SplitRatio = 0.7)
train.data <- subset(iris.norm,spl == T)
test.data <- subset(iris.norm,spl == F)

#