# http://stackoverflow.com/questions/12436902/overlaying-clustering-results-on-an-ordination

require(vegan)
data <- read.csv("F:/LPI/Output/USGSLPIPercentCover.csv",header=TRUE, row.names=1)

dij <- vegdist(data) ## bray curtis dissimilarity
### K-Means ###

# Determine number of clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 4 (or 6?)
# Apply k-means with k=4 (then try 6)
k <- kmeans(data, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(data, col=k$clust, pch=16)

set.seed(2) ## setting a seed to make this reproducible
ord <- metaMDS(data)

col <- c("red2", "green4", "mediumblue", "sienna")
col[k$cluster]


plot(ord, type = "n", display = "sites")
points(ord, col = col[k$cluster], bg = col[k$cluster], pch = 21)
legend("topright", legend = paste("Cluster", 1:4),
       col = col, pt.bg = col, bty = "n", pch = 21)
plot(sig.fit.env,col="blue", cex=0.7,font=2)

ordihull(ord, groups = k$cluster, display = "sites")




# read in Soil environmental data
#data.env <- read.csv("F:/Soils/SoilEnvironmentalDataModWithColbyAWS.csv",header=TRUE, row.names=1)
data.env <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
data.env[is.na(data.env)] <- 0 # replace NA with 0


#function "envfit" fits environmental vectors or factors onto an ordination.
#requires ordination plot first before plot(fit)
fit.env <- envfit(ord,data.env,perm=1000)
fit.env


### Choose only the significant environmental data
sig.data.env<-data.env[,c(4,11,14,15,24,41)]
sig.fit.env<-envfit(ord,sig.data.env,perm=1000)
sig.fit.env # Check that you pulled up the right factors.


#plotMDS
orditkplot(ord, display="species", col="black", cex=0.7, pcol="gray",pch="+",xlim=c(-2,2),ylim=c(-2,2))
saveRDS(Plot, file="F:/SageNMDSvariables/Plot.Rdata")
Plot <- readRDS("F:/SageNMDSvariables/Plot.Rdata")

plot(Plot)
title(main = "NMDS and Soils")
plot(sig.fit.env,col="blue", cex=0.7,font=2)

