#KNN on prostate cancer----
prostate_data <- read.csv(file.choose(),stringsAsFactors = F)
prostate_data <- prostate_data[,-1]

norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

prostate_norm <- as.data.frame(lapply(prostate_data[,-1],norm))

prostate_train <- prostate_norm[1:70,]
prostate_test <- prostate_norm[71:100,]

library(class)
prostate_pred <- knn(prostate_train,prostate_test,prostate_data[1:70,1],k=6)
table(prostate_pred,prostate_data[71:100,1])

library(gmodels)
CrossTable(prostate_data[71:100,1],prostate_pred,prop.chisq = F)
