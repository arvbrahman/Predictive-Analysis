#KNN on iris ----
iris_data<- iris

norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

iris_norm <- as.data.frame(lapply(iris_data[1:4],norm))

iris_train <- iris_norm[1:130,]
iris_test <- iris_norm[131:150,]

iris_pred <- knn(iris_train,iris_test,iris_data[1:130,5],k=5)
table(iris_pred,iris_data[131:150,5])

CrossTable(iris_data[131:150,5],iris_pred,prop.chisq = F)