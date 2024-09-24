summary(iris)
str(iris)

indexes = sample(150,110)

iris.train <- iris[indexes,]
iris.train
iris.test<- iris[-indexes,]
iris.test
  

target = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
target
tree = rpart(target, data = iris.train, method = "class")
tree

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree)

predict(model,iris.test) 