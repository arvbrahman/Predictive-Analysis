#Required Libraries ----
library(ISLR)     #for Carseats data
library(rpart)
library(tree)
library(rpart.plot)

#Loading and splitting the data ----
data(Carseats)

Carseats$Wsales <- as.factor(ifelse(Carseats$Sales>=7,"Yes","No"))

index <- sample(400,350)
Cs.train <- Carseats[index,]
Cs.test <- Carseats[-index,]

#Separating the target variable ----
target <- Wsales ~ . -Sales

#Training the model ----
Model <- rpart(target,Cs.train,method = "class")
Tree <- tree(target,Cs.train,mindev = 0.01)

#Plotting the tree ----
rpart.plot(Model)
plot(Tree)
text(Tree,pretty = 0)

#Predictions ----
predictions <- predict(Model,Cs.test,type = "class")
predictions
p<- predict(Tree,Cs.test,type = "class")
p

#Pruning ----
set.seed(2)
pruned <- cv.tree(Tree, FUN = prune.misclass)
plot(pruned$size,pruned$dev,type = "b",xlab = "Tree size",ylab = "Deviance")

pruned.tree <- prune.misclass(Tree,best = 9)
plot(pruned.tree)
text(pruned.tree,pretty = 0)