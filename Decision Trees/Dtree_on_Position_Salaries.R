#Required libraries
library(caTools)   #for splitting the data
library(rpart)     #for decision tree
library(rpart.plot)
library(ggplot2)


#Loading the data
Ps <- read.csv(file = "Position_Salaries.csv",stringsAsFactors = F)

#Selecting the fields required
Ps <- Ps[,2:3]

#Splitting the data into train and test
set.seed(3)
split <- sample.split(Ps$Salary,SplitRatio = 0.65)
train.set <- subset(Ps,split==T)
test.set <- subset(Ps,split==F)

#Model
model <- rpart(Salary ~ .,
               train.set,
               control = rpart.control(minsplit = 2, cp = 0.01))
model

#Decision Tree
rpart.plot(model)


#
x_grid <- seq(min(Ps$Level),max(Ps$Level), 0.01)
ggplot(Ps,aes(Level,Salary))+
  geom_point()+
  geom_line()
