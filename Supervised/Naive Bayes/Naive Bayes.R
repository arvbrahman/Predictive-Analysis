#Loading necessary libraries ----
library(dplyr)   #for transforming data and contains pipe operator
library(tm)   #tm: text mining library is used for text cleaning 
library(SnowballC)   #Library for stemming
library(caret)    #Library for splitting data
library(wordcloud)   #library for pictorial representation of frequency
library(e1071)   #package for NaiveBayes
library(gmodels)  #library for crosstable for confusionmatrix

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
mail_corpus <- iconv(mail_Data$text)

mail_corpus_clean<-  Corpus(VectorSource(mail_corpus))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords,stopwords())%>%
  tm_map(stripWhitespace)%>%
  tm_map(stemDocument)

#DTM
mail_dtm <- DocumentTermMatrix(mail_corpus_clean)

#Splitting the data ----
set.seed(123)
trainIndex <- createDataPartition(mail_Data$type, p = 0.8, list = F)
mail_train_data <- mail_dtm[trainIndex, ]
mail_test_data <- mail_dtm[-trainIndex, ]
mail_train_labels <- mail_Data$type[trainIndex]
mail_test_labels <- mail_Data$type[-trainIndex]


#Wordcloud ----
wordcloud(mail_corpus_clean,min.freq = 50,
          random.order = F,colors = brewer.pal(5,"Dark2"))

#Frequency
findFreqTerms(mail_train_data,5)
mail_freq_words <- findFreqTerms(mail_train_data, 5)
str(mail_freq_words)
mail_freq_train<- mail_train_data[ , mail_freq_words]
mail_freq_test <- mail_test_data[ , mail_freq_words]

#Training the model ----
model <- naiveBayes(as.matrix(mail_freq_train),mail_train_labels)
predictions <- predict(model,as.matrix(mail_test_data))

#Evaluating ----
confusionMatrix(predictions,mail_test_labels)

CrossTable(predictions,mail_test_labels,prop.chisq = F)

