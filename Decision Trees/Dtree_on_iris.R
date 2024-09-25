#Libraries
library(rpart)
library(rpart.plot)

#Analyze
summary(iris)
str(iris)

#Splitting
indexes = sample(150,110)

iris.train <- iris[indexes,]
iris.train
iris.test<- iris[-indexes,]
iris.test
  
#Separate target variable(Dependent variable)
target = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
target

#Train model
tree = rpart(target, data = iris.train, method = "class")
tree

#Plot the tree used
rpart.plot(tree)

#Check predictions on test data
predict(tree,iris.test) 
