#Loading necessary libraries ----
library(dplyr)
library(tm)   #tm: text mining library is used for text cleaning 
library(SnowballC)
library(caret)
library(e1071)
library(gmodels)

#Loading the data ----
mail_Data <- read.csv(file = "spam.csv",stringsAsFactors = F)

#Observing the data ----
str(mail_Data)
table(mail_Data$type)

#cleaning and transforming the data ----
#Removing unnecessary columns &
#converting label into categorical variable which makes it easy for classification
mail_Data <- mail_Data[,c("type","text")]
mail_Data$type <- as.factor(mail_Data$type)

#Data Preprocessing ----
#text cleaning in this data set 
mail_corpus <- iconv(mail_Data$text, to = "UTF-8", sub = "")

mail_corpus_clean<-  Corpus(VectorSource(mail_corpus))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords,stopwords("en"))%>%
  tm_map(stripWhitespace)%>%
  tm_map(stemDocument)

#DTM
mail_dtm <- DocumentTermMatrix(mail_corpus_clean)

#Splitting the data ----
set.seed(123)
trainIndex <- createDataPartition(mail_Data$type, p = 0.7, list = F)
mail_train_data <- mail_dtm[trainIndex, ]
mail_test_data <- mail_dtm[-trainIndex, ]
mail_train_labels <- mail_Data$type[trainIndex]
mail_test_labels <- mail_Data$type[-trainIndex]

common_features <- intersect(colnames(mail_train_data), colnames(mail_test_data))
mail_train_data <- mail_train_data[, common_features]
mail_test_data <- mail_test_data[, common_features]

#Training the model ----
model <- naiveBayes(as.matrix(mail_train_data),mail_train_labels)
predictions <- predict(model,as.matrix(mail_test_data))

#Evaluating ----
confusionMatrix(predictions,mail_test_labels)

CrossTable(predictions,mail_test_labels,prop.chisq = F)
