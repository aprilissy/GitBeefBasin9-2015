# PCA before K-Means 07/14/2016
# April without USGS

# LPI Data
data <- read.csv("F:/LPI/Output/AprilLPIPercentCover.csv",header=TRUE, row.names=1)


# plot variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
par(mfrow=c(1,2))
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.5)
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.5, log='x')
par(mar=mar)

# Scale
data2 <- data.frame(scale(data))
# Verify variance is uniform
plot(sapply(data2, var))



# Proceed with principal components
par(mfrow=c(1,1))
pc <- princomp(data2)
plot(pc)
plot(pc, type='l')
summary(pc) # 2 components is 'elbow' but does not explain >85% variance(would need 21 components)(only explains about 22%)


# First few principal components
comp <- data.frame(pc$scores[,1:2])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))


library(rgl)
# # Multi 3D plot
# plot3d(comp$Comp.1, comp$Comp.2, comp$Comp.3)


### K-Means ###

# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 4 
# Apply k-means with k=4 
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)


# 3D plot
# plot3d(comp$Comp.1, comp$Comp.2, comp$Comp.3, col=k$clust)




# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# First cluster
C1<-row.names(data[k$clust==clust[1],]);C1
# Second Cluster
C2<-row.names(data[k$clust==clust[2],]);C2
# Third Cluster
C3<-row.names(data[k$clust==clust[3],]);C3
# Fourth Cluster
C4<-row.names(data[k$clust==clust[4],]);C4
# # Fifth Cluster
# C5<-row.names(data[k$clust==clust[5],]);C5
# # Sixth Cluster
# C6<-row.names(data[k$clust==clust[6],]);C6


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

boxplot(data$JUOS ~ k$cluster,
        xlab='Cluster', ylab='JUOS',
        main='JUOS by Cluster')



## to hold the factor for whether a site is infested or control
tlabs <- c("C1","C2","C3","C4")

treat <- factor(c(C1,C2,C3,C4), levels = tlabs)





treat <- factor(rep(tlabs, each = 3), levels = tlabs)
## setting the levels explicitly insures R doesn't reorder them alphabetically



library(vegan)
library(MASS)
library(colorspace)

data <- read.csv("F:/LPI/Output/USGSLPIPercentCover.csv",header=TRUE, row.names=1)
e.data.mds<-metaMDS(comm=data,distance="euc",trace=FALSE)
e.data.mds #.09
plot.sc = scores(e.data.mds)
# Stress <0.10 indicates that the ordination is good "with no real 
# risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances
stressplot(e.data.mds) 

data.env <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
data.env[is.na(data.env)] <- 0 # replace NA with 0

fit.env <- envfit(e.data.mds,data.env,perm=1000)
fit.env

### Choose only the significant environmental data
sig.data.env<-data.env[,c(4,11,14,15,41)]
sig.fit.env<-envfit(e.data.mds,sig.data.env,perm=1000)
sig.fit.env # Check that you pulled up the right factors.

#plotMDS
orditkplot(e.data.mds, display="species", col="black", cex=0.7, pcol="gray",pch="+",xlim=c(-0.4,0.25),ylim=c(-0.2,0.25))
saveRDS(Plot, file="F:/SageNMDSvariables/Plot.Rdata")
Plot <- readRDS("F:/SageNMDSvariables/Plot.Rdata")

plot(Plot)
title(main = "NMDS and Soils")
plot(sig.fit.env,col="blue", cex=0.7,font=2)

plot(e.data.mds,type="t",main="NMDS Soil Data")
plot(sig.fit.env,col="blue", cex=0.7)

