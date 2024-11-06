#Libraries ----
library(psych)

#Loading the dataset ----
ins <-  read.csv(file = "insurance.csv",stringsAsFactors = F)
ins$age2 <- ins$age^2
ins$bmi30 <- ifelse(ins$bmi>=30,1,0)

#Modelling ----
model <- lm(charges ~ . ,ins)

#Analyzing ----
hist(ins$age)
table(ins$region)
cor(ins[c("age","bmi","children","charges")])
describe(ins)
pairs(ins[c("age","bmi","children","charges")])
pairs.panels(ins[c("age","bmi","children","charges")])

#Prediction ----
df2<- data.frame(age=25,sex="female",bmi=30.2,children=2,smoker="no",region="northeast",age2=625,bmi30=1)
predict(model,df2)
