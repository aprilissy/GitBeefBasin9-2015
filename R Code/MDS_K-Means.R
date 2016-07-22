# MDS before K-Means 05/11/2016

# LPI Data
data <- read.csv("F:/LPI/Output/USGSLPIPercentCover.csv",header=TRUE, row.names=1)


# plot variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
# par(mfrow=c(1,2))
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.5)
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.5, log='x')
par(mar=mar)

######## NMDS #########

library(vegan)
library(MASS)
library(colorspace)

ord<-metaMDS(comm=data,distance="bray",trace=FALSE)
ord #.17
plot.sc = scores(ord)
# Stress <0.10 indicates that the ordination is good "with no real 
# risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances
stressplot(ord) 


### K-Means ###

# Determine number of clusters
wss <- (nrow(ord$points)-1)*sum(apply(ord$points,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ord$points,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




# From scree plot elbow occurs at k = 3 (or 8?)
# Apply k-means with k=3 (then try 8)
k <- kmeans(ord$points, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(ord$points, col=k$clust, pch=16)
legend("topright",c("Cluster 1", "Cluster 2","Cluster 3")
       ,pch=16, col=k$cluster)




# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])



# Compare accommodation by cluster in boxplot
boxplot(data$KRLA2 ~ k$cluster,
        xlab='Cluster', ylab='KRLA2',
        main='KRLA2 by Cluster')

par(mfrow=c(1,2))
boxplot(data$ARTR2 ~ k$cluster,
        xlab='Cluster', ylab='ARTR2',
        main='ARTR2 by Cluster')

boxplot(data$ARTR2.D ~ k$cluster,
        xlab='Cluster', ylab='ARTR2.D',
        main='ARTR2.D by Cluster')

boxplot(data$ATCA2 ~ k$cluster,
        xlab='Cluster', ylab='ATCA2',
        main='ATCA2 by Cluster')

boxplot(data$ATCA2.D ~ k$cluster,
        xlab='Cluster', ylab='ATCA2.D',
        main='ATCA2.D by Cluster')

boxplot(data$BOGR2 ~ k$cluster,
        xlab='Cluster', ylab='BOGR2',
        main='BOGR2 by Cluster')

boxplot(data$BOGR2.D ~ k$cluster,
        xlab='Cluster', ylab='BOGR2.D',
        main='BOGR2.D by Cluster')

boxplot(data$SPCR ~ k$cluster,
        xlab='Cluster', ylab='SPCR',
        main='SPCR by Cluster')

boxplot(data$SPCR.D ~ k$cluster,
        xlab='Cluster', ylab='SPCR.D',
        main='SPCR.D by Cluster')

boxplot(data$HECO26 ~ k$cluster,
        xlab='Cluster', ylab='HECO26',
        main='HECO26 by Cluster')

boxplot(data$HECO26.D ~ k$cluster,
        xlab='Cluster', ylab='HECO26.D',
        main='HECO26.D by Cluster')

boxplot(data$OPPO ~ k$cluster,
        xlab='Cluster', ylab='OPPO',
        main='OPPO by Cluster')

boxplot(data$OPPO.D ~ k$cluster,
        xlab='Cluster', ylab='OPPO.D',
        main='OPPO.D by Cluster')

boxplot(data$PIED ~ k$cluster,
        xlab='Cluster', ylab='PIED',
        main='PIED by Cluster')

boxplot(data$PIED.D ~ k$cluster,
        xlab='Cluster', ylab='PIED.D',
        main='PIED.D by Cluster')

boxplot(data$JUOS ~ k$cluster,
        xlab='Cluster', ylab='JUOS',
        main='JUOS by Cluster')



### Add in Soils Varibles ###
data.env <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
data.env[is.na(data.env)] <- 0 # replace NA with 0

fit.env <- envfit(ord,data.env,perm=1000)
fit.env

### Choose only the significant environmental data
sig.data.env<-data.env[,c(4,11,14,24,27)]
sig.fit.env<-envfit(ord,sig.data.env,perm=1000)
sig.fit.env # Check that you pulled up the right factors.


### Plot Soils and Ordihull ###
plot(ord$points, col=k$clust, pch=16)
legend("topright",c("Cluster 1", "Cluster 2","Cluster 3")
       ,pch=16, col=k$cluster)
plot(sig.fit.env,col="blue", cex=0.7,font=2)
ordihull(ord, groups = k$clust, display = "sites")

