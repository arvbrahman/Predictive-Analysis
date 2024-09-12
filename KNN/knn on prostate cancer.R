#KNN on prostate cancer----
prostate_data <- read.csv(file.choose(),stringsAsFactors = F)
prostate_data <- prostate_data[,-1]

#Function for normalization
norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

prostate_norm <- as.data.frame(lapply(prostate_data[,-1],norm))  #standardizing

#Splitting the Data into train and test
prostate_train <- prostate_norm[1:70,]
prostate_test <- prostate_norm[71:100,]

#KNN
library(class)  #class library conatins knn
prostate_pred <- knn(prostate_train,prostate_test,prostate_data[1:70,1],k=6)
table(prostate_pred,prostate_data[71:100,1])

#Evaluation
library(gmodels) #gmodels conatins crosstable
CrossTable(prostate_data[71:100,1],prostate_pred,prop.chisq = F)
