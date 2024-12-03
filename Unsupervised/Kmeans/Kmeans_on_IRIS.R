#Required Packages ----
library(cluster)  #for plotting clusters
library(ggplot2)  #for plotting clusters

#Working on iris dataset ----
iris
iris_new <- iris[1:4]      #Removing the 5th column Species


#Training the model ----
set.seed(123)
model <- kmeans(iris_new, centers = 3,nstart = 20)
model


#Analyzing the gained clusters
model$cluster
cm <- table(iris$Species,model$cluster)
cm

#Plotting the clusters ----
plot(iris_new$Sepal.Length,iris_new$Sepal.Width,
     col = model$cluster,
     main = "K-means with 3 clusters")

#Adding centroids 
model$centers
points(model$centers[,c("Sepal.Length","Sepal.Width")],
       col = 1:3,
       pch = 7, #Symbol
       cex = 3)   #Font size

#Plotting with shaded clusters with cluster library ----
clusplot(iris_new[,c("Sepal.Length","Sepal.Width")],
         model$cluster,
         lines = 1,
         color = T,
         shade = T,
         labels = 1,
         plotchar = T,
         span = T,
         main = paste("cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')


#plotting clusters with ggplot2 library ----
iris_new$Cluster <- model$cluster

ggplot(iris_new, aes(x = Sepal.Length, y = Sepal.Width, color = factor(Cluster))) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal()
