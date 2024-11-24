#Required Libraries
library(class)       #for knn function

library(caTools)   #for splitting

#Working on `Mobile Device Usage and User Behavior Dataset`
Mobile <- read.csv(file.choose(),stringsAsFactors = F)
Mobile<-Mobile[,-1]
str(Mobile)

names(Mobile)
table(Mobile$User.Behavior.Class)
hist(Mobile$User.Behavior.Class)

#Splitting the data
set.seed(123)
spl<- sample.split(Mobile$User.Behavior.Class,
                   SplitRatio = 0.8)
Mob.train <- subset(Mobile,spl==T)
Mob.test <- subset(Mobile,spl==F)

#Removing the unnecessary columns 
keeps = c("App.Usage.Time..min.day.", 
          "Screen.On.Time..hours.day.",  
          "Battery.Drain..mAh.day.",
          "Number.of.Apps.Installed",
          "Data.Usage..MB.day.",
          "Age", "User.Behavior.Class")

Mob.train<- Mob.train[keeps]
Mob.test<- Mob.test[keeps]

#Training the model with knn
Model <- knn(Mob.train,Mob.test,Mob.train$User.Behavior.Class,k=10)
table(Model,Mob.test$User.Behavior.Class)