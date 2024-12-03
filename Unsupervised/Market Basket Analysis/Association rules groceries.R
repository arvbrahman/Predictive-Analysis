#Libraries required
library(arules)    #for apriori

#Reading and loading the data as transactions
Transactions <- read.transactions(file= "groceries.csv")

#Analyzing
str(Transactions)
summary(Transactions)
inspect(Transactions[1:10])

#Plotting
itemFrequency(Transactions[,1:3])
itemFrequencyPlot(Transactions, topN = 20)

image(Transactions[1:5])
image(sample(Transactions,100))

#Training the model
apriori(Transactions)

Model.rules <- apriori(Transactions,
                 parameter =list(support = 0.006, 
                                 confidence = 0.25, minlen = 2))
Model.rules
summary(Model.rules)
inspect(Model.rules[1:3])
inspect(sort(Model.rules, by="lift")[1:5])

#Saving the Association rules in csv format
berryrules <- subset(Model.rules, items %in% "berries")
inspect(berryrules)

write(Model.rules, file = "Groceryrules.csv",
      sep = ",", quote = T, row.names = F)

Rules.df <- as(Model.rules,"data.frame")
str(Rules.df)