# K-Means Cluster Analysis 05/10/2016

# LPI Data
data <- read.csv("F:/LPI/Output/USGSLPIplotXspp.csv",header=TRUE, row.names=1)


# Check for the optimal number of clusters given the data
mydata <- data
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(data, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

op <- par(oma=c(5,7,1,1))
par(mar = rep(2, 4))

# Plot results
plot(data, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
par(op)