#Working on the iris dataset
iris
new.iris <- iris[1:4]    #removing the labeled column

#Euclidean distance
d = dist(new.iris,method = "euclidean")

#Training the model
model <- hclust(d,method = "average")
model

#plotting the dendrogram
plot(model)

#Dividing the clusters into 4
grps = cutree(model,k=4)
grps

rect.hclust(model,k=2,border = "blue")
rect.hclust(model,k=4,border = "yellow")
