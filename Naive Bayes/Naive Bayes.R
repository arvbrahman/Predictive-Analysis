#Loading the data
mail_Data <- read.csv(file = "spam.csv",stringsAsFactors = F)

#Observing the data 
str(mail_Data)
table(mail_Data$type)

#Making categorical variable
mail_Data$type <- as.factor(mail_Data$type)
str(mail_Data)

#Data Cleaning
library(tm)
mail_corpus <- iconv(mail_Data$text)
mail_corpus <- Corpus(VectorSource(mail_corpus))
lapply(mail_corpus, as.character)
inspect(mail_corpus[1:2])

mail_clean <- tm_map(mail_corpus,removePunctuation)
inspect(mail_clean[1:2])
mail_clean <-  tm_map(mail_clean,removeNumbers)
mail_clean <- tm_map(mail_clean,tolower)
inspect(mail_clean[1:2])
mail_clean <- tm_map(mail_clean,removeWords,stopwords())
inspect(mail_clean[1:2])
mail_clean <- tm_map(mail_clean,trimws)
inspect(mail_clean[1:2])


#Stemming
library(SnowballC)
mail_clean <- tm_map(mail_clean,stemDocument)
inspect(mail_clean[1:2])
mail_dtm <- DocumentTermMatrix(mail_clean)

mail_Train <- mail_dtm[1:4169,]
mail_Test <- mail_dtm[4170:5572,]
mail_Train_lab <- mail_Data[1:4169,]$type
mail_Test_lab <- mail_Data[4170:5572,]$type


library(wordcloud)
wordcloud(mail_clean,min.freq = 50,
          random.order = F,
          colors = brewer.pal(5,"Dark2"))


install.packages("e1071")
library("e1071")
model <-  naiveBayes(mail_Train,mail_Test,mail_Train_lab)

help(naiveBayes)
