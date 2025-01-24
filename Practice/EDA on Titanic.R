#Taking titanic dataset from kaggle ----
#All points and  variable are from that data only
a<- read.csv(file.choose(), stringsAsFactors = F)

summary(a)
which(is.na(a$Age))
a[153,"Fare"]

#omitting NA values
n <-complete.cases(a)
a[n,]
a<- a[n,]

#separating features
keeps<- c("Pclass","Sex","Age","SibSp","Parch","Embarked","Fare")
df <- a[keeps]

#omitting the unnecessary variables or columns
#Scaling(removing the median then dividing by std)
library(tidyverse)     #Collection of r packages (tidyr, ggplot2, dplyr, etc.)
df_scaled <- df %>%
  select(-Sex, -Embarked) %>%
  scale()

#visualization ----
#Histogram for continuous variables(Age,Fare)
ggplot(df,aes(x=Age))+
  geom_histogram(binwidth = 5, fill = "skyblue", color = "red")+
  theme_dark()

ggplot(df,aes(x=Fare))+
  geom_histogram(binwidth = 30, fill = "orange", color = "blue")+
  labs(title = "Distribution of Fare") +
  theme_linedraw()

#bar plot mostly for categorical variables (Sex,Embarked,Pclass)
ggplot(df,aes(Sex))+
  geom_bar(fill = c("pink","blue"),color = "red") +
  theme_linedraw()

ggplot(df,aes(Embarked))+
  geom_bar(fill = "steelblue") +
  theme_linedraw()

ggplot(df,aes(Pclass))+
  geom_bar(fill = c("tomato","orange","red")) +
  theme_linedraw()

#Boxplot for comparisons between continuous variables and categorical variables
ggplot(df,aes(factor(Pclass),y=Age))+
  geom_boxplot(fill = "steelblue",color = "red")+
  ylim(0,80)

ggplot(df,aes(factor(Pclass),y=Fare))+
  geom_boxplot(fill = "yellow",color = "tomato")+
  scale_y_continuous(breaks = seq(0,515,by=100))+
  theme_linedraw()

#Scatter Plots shows relationship between continuous variables
ggplot(df,aes(Age,Fare,colour = factor(Pclass)))+
  geom_point()+
  scale_color_manual(values = c("red", "green", "blue"))+
  theme_bw()

ggplot(df,aes(Fare,Age,,color = factor(Sex)))+
  geom_point()+
  scale_color_manual(values = c("lightpink","lightblue"))+
  theme_dark()