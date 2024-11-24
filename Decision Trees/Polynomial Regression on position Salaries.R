#Packages Required
library(ggplot2)

#loading the dataset
possal <- read.csv(file = "Position_Salaries.csv",stringsAsFactors = F)


#Analyzing
str(possal)
summary(possal)
table(possal$Position)
hist(possal$Level)
View(possal)

#Selecting 2nd and 3rd column
pos <- possal[2:3]

#Linear Regression Model
lm.model <- lm(Salary ~ .,pos)
summary(lm.model)


#Fitting some variables for Polynomial Regression 
pos$Level2 <- pos$Level^2
pos$Level3 <- pos$Level^3
pos$Level4 <- pos$Level^4

#Polynomial Regression Model
poly.model <- lm(Salary ~ . ,pos)
summary(poly.model)


#Visualizing both the models
ggplot()+
  geom_point(aes(Level,Salary),pos,colour = "green")+
  geom_line(aes(Level,predict(lm.model,pos)),pos,colour = "orange")+
  ggtitle("Truth or Bluff(Linear Regression)")+
  theme_dark()

ggplot()+
  geom_point(aes(Level,Salary),pos,colour = "green")+
  geom_line(aes(Level,predict(poly.model,pos)),pos,colour = "orange")+
  ggtitle("Truth or Bluff(Polynomial Regression)")+
  theme_dark()


#Visualizing the Regression Model results (for higher resolution and smoother curve)
x_grid <- seq(min(pos$Level),max(pos$Level),0.1)

ggplot()+
  geom_point(aes(Level,Salary),
             pos,colour = "blue")+
  geom_line(aes(x_grid,predict(poly.model,data.frame(Level = x_grid,
                                                     Level2 = x_grid^2,
                                                     Level3 = x_grid^3,
                                                     Level4 = x_grid^4))),
            colour = "red")+
  ggtitle("Polynomial Regression")+
  theme_dark()


#Interpolation Predicting
predict(lm.model,data.frame(Level=6.5))
predict(poly.model,data.frame(Level=6.5,
                              Level2=6.5^2,
                              Level3=6.5^3,
                              Level4=6.5^4))