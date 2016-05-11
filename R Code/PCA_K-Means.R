# PCA before K-Means 05/11/2016

# LPI Data
data <- read.csv("F:/LPI/Output/USGSLPIPercentCover.csv",header=TRUE, row.names=1)


# plot variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.5)
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.5, log='x')
par(mar=mar)

# Scale
data2 <- data.frame(scale(data))
# Verify variance is uniform
plot(sapply(data2, var))


# Proceed with principal components
pc <- princomp(data2)
plot(pc)
plot(pc, type='l')
summary(pc) # 3 components is 'elbow' but does not explain >85% variance(would need 23 components)(only explains about 7%)


# First four principal components
comp <- data.frame(pc$scores[,1:3])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))


library(rgl)
# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2, comp$Comp.3)


### K-Means ###

# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 4 (or 6?)
# Apply k-means with k=4 (then try 6)
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)


# 3D plot
plot3d(comp$Comp.1, comp$Comp.2, comp$Comp.3, col=k$clust)




# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])
# Fourth Cluster
row.names(data[k$clust==clust[4],])
# Fifth Cluster
row.names(data[k$clust==clust[5],])
# Sixth Cluster
row.names(data[k$clust==clust[6],])





# Compare accommodation by cluster in boxplot
boxplot(data$ARTR2 ~ k$cluster,
        xlab='Cluster', ylab='KRLA',
        main='KRLA by Cluster')



