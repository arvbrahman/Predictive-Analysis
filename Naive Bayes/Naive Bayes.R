#Loading necessary libraries
library(tm)   #tm: text mining library is used for text cleaning 
library(caret)
library(e1071)
library(dplyr)

#Loading the data
mail_Data <- read.csv(file = "spam.csv",stringsAsFactors = F)

#Observing the data 
str(mail_Data)
table(mail_Data$type)

#Removing unnecessary columns &
#converting label into categorical variable which makes it easy for classification
mail_Data <- mail_Data[,c("type","text")]
mail_Data$type <- as.factor(mail_Data$type)
str(mail_Data)


#Data Cleaning Or text cleaning in this data set 
mail_corpus_clean <- iconv(mail_Data$text)%>%
  Corpus(VectorSource)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords,stopwords("en"))



#Stemming
library(SnowballC)
mail_clean <- tm_map(mail_clean,stemDocument)
inspect(mail_clean[1:2])
mail_dtm <- DocumentTermMatrix(mail_clean)


