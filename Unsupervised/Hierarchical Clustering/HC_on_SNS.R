#Loading the data
sns <- read.csv(file = "snsdata.csv",stringsAsFactors = F)
sns_new <- sns[1:200,3:40]    #removing the labelled columns


#Model training 
e_dist <- dist(sns_new,method = "euclidean")
Model <- hclust(e_dist,method = "average")


#Plotting the dendrogram
plot(Model)

rect.hclust(Model,k = 2 , border = "gold")
rect.hclust(Model,k = 3,border = "blue")