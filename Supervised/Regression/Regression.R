#
h<- c(6.1,9.1,6.9,5.6,6.7,8.9,7.8,6.8,7.9,6.6)
w<- c(55,56,67,69,78,89,78,56,56,60)

#
relation<- lm(w~h)
relation

df <- data.frame(h=c(5.6,6.2,6.4,5.3))
predict(relation,df)