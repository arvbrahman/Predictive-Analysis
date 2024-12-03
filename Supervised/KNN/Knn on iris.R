#KNN on iris ----
iris_data<- iris

#defining the function for normalization
norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#standardizing the data
iris_norm <- as.data.frame(lapply(iris_data[1:4],norm))
iris.norm <- scale(iris_data[1:4])     #Standardizing through scaling

#splitting into train and test data set
iris_train <- iris_norm[1:130,]
iris_test <- iris_norm[131:150,]

#applying knn model
library(class)
iris_pred <- knn(iris_train,iris_test,iris_data[1:130,5],k=5)

#Checking the accuracy in the table form
table(iris_pred,iris_data[131:150,5])
library(gmodels)
CrossTable(iris_data[131:150,5],iris_pred,prop.chisq = F)