data <- read.csv(file = "wbcd.csv",stringsAsFactors = F)   #Loading the original data

wbcd <- data[,-1]     #removing the first column and saving it 

data_norm <- function(x){    #Used to normalize the data between 0 and 1
  ((x-min(x))/(max(x)-min(x)))       
}

wbcd_norm <- as.data.frame(lapply(wbcd[,-1], data_norm)) 
str(wbcd_norm)


#separating data for training and testing
wbcd_train <- wbcd_norm[1:450,]
wbcd_test <- wbcd_norm[451:569,]

library(class)       #the package that contains Knn function
wbcd_pred <- knn(wbcd_train,wbcd_test,wbcd[1:450,1],k=24)
table(wbcd_pred,wbcd[451:569,1])


#To check the results on a Crosstable
install.packages("gmodels")
library(gmodels)
CrossTable(wbcd[451:569,1],wbcd_pred,prop.chisq = F)