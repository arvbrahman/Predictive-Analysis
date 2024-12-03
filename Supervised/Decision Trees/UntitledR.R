#you are analzing a dataset from CTG examination which recored FM:no of feta movement 
#per second,LB:FHR baseline (beats per minute) and uterine conatction (uc) measurments
#,ASTV:percenatge of time with abnormal short tem variability,Nsp is target class ,where=1 normal,2=suspect ,and 3=pathological,
#your goal is to build a decisson tree classfier to predict fetal state based on provide features .Evaluate model accuracy and visualise decission trees.
#dataset : "cardiotocographic"

#Required libraries
library(caTools)   #for splitting the data
library(rpart)     #for decision tree
library(rpart.plot)
library(ggplot2)


#Loading the data
data <- read.csv(file = "Cardiotocographic.csv",stringsAsFactors = F)

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