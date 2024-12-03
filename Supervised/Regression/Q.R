#Q1 ----
#Create a dataframe
house <- data.frame(size=c(1500,1800,2000,2200,2500,3000,3500,4000,4500,5000),
                    bedrooms=c(3,3,4,4,5,5,5,6,6,6),
                    bathrooms=c(2,2,3,3,4,4,3,5,4,5),
                    age_years=c(10,5,8,12,6,3,15,7,4,2),
                    price=c(300000,450000,350000,420000,500000,600000,580000,600000,750000,800000))


#Linear Regression model
model<- lm(price ~ .,house)
model


#Predicting
df<- data.frame(size=3200,bedrooms=5,bathrooms=4,age_years=8)
predicted<- predict(model,df)

cat("Predicted selling price: ",predicted,"/n")



#Q2 ----
#Creating a dataframe
emp <-  data.frame(exp=c(1,2,3,4,5,6,7,8,9,10),
                  edu=c(12,14,16,12,16,14,12,14,16,12),
                  age=c(21,23,24,25,26,23,27,28,27,25),
                  salary=c(50,55,60,62,58,65,68,70,72,75))


#Model (Linear Regression)
model<- lm(salary ~ .,emp)
model


#Prediction
df <- data.frame(exp=4,edu=16,age=24)
predicted<- predict(model,df)
cat("Predicted salary for the employee: $",predicted,"\n")