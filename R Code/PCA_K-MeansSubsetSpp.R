# PCA before K-Means 05/11/2016

# LPI Data
data <- read.csv("F:/LPI/Output/USGSLPIPercentCover.csv",header=TRUE, row.names=1)
data.i <- subset(data, select = c(SPCR,PIED,LAOC3,OPPO,KRLA2,JUOS,HECO26,GUSA2,GUSA2.D,CHVI8,BRTE,BOGR2.D,BOGR2,Bare.Soil,ATCA2.D,ATCA2,ARTR2,ARTR2.D,AMID,ACHY) )
data.g <- subset(data, select = c(SPCR,HECO26,BRTE,BOGR2.D,BOGR2,Bare.Soil,ACHY) )
data.s <- subset(data, select = c(PIED,KRLA2,JUOS,CHVI8,Bare.Soil,ATCA2.D,ATCA2,ARTR2,ARTR2.D) )
data.f <- subset(data, select = c(LAOC3,OPPO,Bare.Soil,AMID) )


# plot variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(data.s, var), horiz=T, las=1, cex.names=0.5)
barplot(sapply(data.s, var), horiz=T, las=1, cex.names=0.5, log='x')
par(mar=mar)

# Scale
data2 <- data.frame(scale(data.s))
# Verify variance is uniform
plot(sapply(data2, var))


# Proceed with principal components
pc <- princomp(data2)
plot(pc)
plot(pc, type='l')
summary(pc) # 3 components is 'elbow' but does not explain >85% variance(would need 23 components)(only explains about 7%)


# First four principal components
comp <- data.frame(pc$scores[,1:4])

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
k <- kmeans(comp, 6, nstart=25, iter.max=1000)
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
boxplot(data$ATCA2 ~ k$cluster,
        xlab='Cluster', ylab='ATCA2',
        main='ATCA2 by Cluster')





