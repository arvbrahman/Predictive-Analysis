#Loading necessary libraries ----
library(dplyr)   #for transforming data and contains pipe operator
library(tm)   #tm: text mining library is used for text cleaning 
library(SnowballC)   #Library for stemming
library(caret)    #Library for splitting data
library(wordcloud)   #library for pictorial representation of frequency
library(e1071)   #package for NaiveBayes
library(gmodels)  #library for crosstable for confusionmatrix


#loading data ----
sms_raw <- read.csv(file = "spam.csv", stringsAsFactors = FALSE) 


#Cleaning  ----
sms_raw$type <- factor(sms_raw$type)
sms_raw <- sms_raw[,c("type","text")]


#Text Cleaning: ----
sms_corpus<-iconv(sms_raw$text)
sms_corpus_clean <- Corpus(VectorSource(sms_corpus))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, stopwords())%>%
  tm_map(removePunctuation)%>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)


#Document-Term Matrix (DTM) & splitting ----
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- sms_raw[1:4169, ]$type 
sms_test_labels <- sms_raw[4170:5559, ]$type


#Word Cloud ----
wordcloud(sms_corpus_clean, min.freq = 50, 
          random.order = FALSE,colors=brewer.pal(5,"Dark2")) 

#findFreqTerms
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#convert_counts ----
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)


#Model training ----
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)


#CrossTable ----
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual')) 
confusionMatrix(sms_test_pred,sms_test_labels)



#ti | fp
#fn | tn     accuracy formula