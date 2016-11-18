# Health Means on MDS before K-Means 11/17/2016

# LPI Data
data <- read.csv("F:/LPI/Output/AprilLPIPercentCover.csv",header=TRUE, row.names=1)

######## NMDS #########

library(vegan)
library(MASS)
library(colorspace)

ord<-metaMDS(comm=data,distance="euc",trace=FALSE)
ord #.195
plot.sc = scores(ord)
# Stress <0.10 indicates that the ordination is good "with no real 
# risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances
stressplot(ord) 
gof <- goodness(ord)

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
       ,pch=16, col=unique(k$cluster))





# Cluster sizes
table(k$clust)
clust <- names(table(k$clust))


# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])



# Pull Out Cluster Plot Names
Clust1 <- row.names(data[k$clust==clust[1],])
Clust2 <- row.names(data[k$clust==clust[2],])
Clust3 <- row.names(data[k$clust==clust[3],])


# Health Data

LA<-read.csv("F:/Health/LeafAreaEpidermalConductance.csv",header=TRUE)
LA <- LA[ which(!LA$Wet.Dry<0.000000), ] # Remove negative weights.
LA <- LA[,c(1,14:15)]

NP<-read.csv("F:/Health/April Sagebrush N and Protein.csv",header=TRUE)
NP <- NP[,c(2,5:6)]

# find means from LA
pdw <- ddply( LA, 'Plot', summarize, pdw = mean(PctDryWeight, na.rm = T))
sla <- ddply( LA, 'Plot', summarize, sla = mean(SLAcm, na.rm = T))
LA <- join(pdw, sla, by = 'Plot', type = 'inner')

# What plots do LA and NP have in common?
LA.NP <- merge(NP, LA, by=c("Plot")) 


# Find which Health rows are in which clusters
LAC1 <- LA[LA$Plot %in% Clust1,]
LAC2 <- LA[LA$Plot %in% Clust2,]
LAC3 <- LA[LA$Plot %in% Clust3,]

NPC1 <- NP[NP$Plot %in% Clust1,]
NPC2 <- NP[NP$Plot %in% Clust2,]
NPC3 <- NP[NP$Plot %in% Clust3,]


# Find the Mean of each column (each health variable) 
MeanLAC1 <- colMeans(LAC1[,c(2,3)])
MeanLAC2 <- colMeans(LAC2[,c(2,3)])
MeanLAC3 <- colMeans(LAC3[,c(2,3)])

MeanNPC1 <- colMeans(NPC1[,c(2,3)])
MeanNPC2 <- colMeans(NPC2[,c(2,3)])
MeanNPC3 <- colMeans(NPC3[,c(2,3)])


# Combine means of LA and NP into one string
MeanC1 <- c(MeanLAC1,MeanNPC1)
MeanC2 <- c(MeanLAC2,MeanNPC2)
MeanC3 <- c(MeanLAC3,MeanNPC3)

# Combine into cluster mean table
Mean <- rbind(MeanC1,MeanC2,MeanC3)



### Plot Soils and Ordihull ###
par(mfrow=c(1,1))
plot(ord$points, col=k$clust, pch=16,
     xlim=c(-0.25,0.3),ylim=c(-0.2,0.35), main="Percent Dry Weight")

legend("topright",c("Cluster 1", "Cluster 2","Cluster 3")
       ,pch=16, col=unique(k$cluster),x.intersp=0.6
       , y.intersp=0.6,bty="n")

ordihull(ord, groups = k$clust, display = "sites"
         ,draw="polygon")

# Percent Dry Weight
text(0.045,0.08, "0.55",pch=3,cex=1.25)
text(0.01,-0.07, "0.43",pch=3,cex=1.25)
text(-0.12,0.01, "0.57",pch=3,cex=1.25)

# Specific Leaf Area
text(0.045,0.08, "640",pch=3,cex=1.25)
text(0.01,-0.07, "768",pch=3,cex=1.25)
text(-0.12,0.01, "718",pch=3,cex=1.25)

# Nitrogen Percent
text(0.045,0.08, "1.93",pch=3,cex=1.25)
text(0.01,-0.07, "2.04",pch=3,cex=1.25)
text(-0.12,0.01, "1.99",pch=3,cex=1.25)

# Protein Percent
text(0.045,0.08, "12.06",pch=3,cex=1.25)
text(0.01,-0.07, "12.78",pch=3,cex=1.25)
text(-0.12,0.01, "12.43",pch=3,cex=1.25)


plot(sig.fit.env,col="blue", cex=0.7,font=2)
plot(v.sig.fit.env,col="green4", cex=0.7,font=2)

