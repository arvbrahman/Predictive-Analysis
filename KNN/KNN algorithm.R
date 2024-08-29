data <- read.csv(file = "wisc_bc_data",stringsAsFactors = F)   #the original data

wdbc <- data[,-1]     #removing the first column and saving it 

data_norm <- function(x){    #Used to normalize the data between 0 and 1
  ((x-min(x))/(max(x)-min(x)))       
}

wdbc_norm <- as.data.frame(lapply(wdbc[,-1], data_norm)) 
summary(wdbc_norm)

#separating data for training and test
wdbc_train <- wdbc_norm[1:450,]
wdbc_test <- wdbc_norm[451:569,]

library(class)       #the package that contains Knn function
wdbc_pred <- knn(wdbc_train,wdbc_test,wdbc[1:450,1],k=24)
table(wdbc_pred,wdbc[451:569,1])


install.packages("gmodels")
library(gmodels)
CrossTable(wdbc[451:569,1],wdbc_pred,prop.chisq = F)