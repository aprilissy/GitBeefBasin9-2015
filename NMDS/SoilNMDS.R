library(vegan)
library(MASS)
library(colorspace)

# read in data
data.env.A <- read.csv("F:/Soils/SoilEnvironmentalDataApril.csv",header=TRUE, row.names=1)
data.env.A[is.na(data.env.A)] <- 0 # replace NA with 0
data.A <- data.env.A[,c(1:40,42:43)]

H1 <- data.A[,c(1:11)]
H2 <- data.A[-c(30,32,38,45,46,69),c(12:29)]
Plot <- data.A[,c(30:42)]
Plot2 <- data.A[,c(11,30:42)]


# add shrub environmental variables
sage.A <- read.csv("F:/SageNMDSvariables/Sage.Env.April.csv",header=TRUE,row.names=1)
sage.A[is.na(sage.A)] <- 0 # replace NA with 0
sage.H2 <- sage.A[-c(30,32,38,45,46,69),]
# # set factors to factor not numeric
# ix <- c(41)
# data.env.A[ix] <- lapply(data.env.A[ix], as.factor) 
# id <- c(1:40,42:43)
# data.env.A[id] <- lapply(data.env.A[id], as.numeric)
# data.A <- data.env.A[,c(1:40,42:43)]

# # look at distance methods
# rankindex(sage.A, data.A, c("euc","man","bray", "jac", "kul"))

#calculate dissimilarities, use function "vegdist"in VEGAN package
# data.dis<-vegdist(data.A,method="bray")
# dis.matrix<-as.matrix(data.dis)
# rankindex(dis.matrix,data.A)


ord<-metaMDS(comm=data.A,distance="bray",trace=FALSE, k=3,
             autotransform = FALSE,trymax = 100, zerodist = "add")
ord # k2=0.99 ~10%  # k3=.074 ~7.5%   #k4=.0585 ~6%   #k5=.048 ~5%  #k6=.0408 ~4%
stressplot(ord)
#Stress <0.10 indicates that the ordination is good "with no real 
#risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances


H1.ord<-metaMDS(comm=H1,distance="bray",trace=FALSE, k=3,
             autotransform = FALSE,trymax = 100, zerodist = "add")
H1.ord # k2=0.105 ~10.5%  # k3=.079 ~8%   #k4=.0645 ~6.5%   #k5=.0557 ~5.6% 
stressplot(H1.ord)


H2.ord<-metaMDS(comm=H2,distance="bray",trace=FALSE, k=3,
             autotransform = FALSE,trymax = 100, zerodist = "ignore")
H2.ord # k2=0.128 ~13%  # k3=0.914 ~9%   #k4=.0709 ~7%   #k5=.0617 ~6% 
stressplot(H2.ord)


Plot.ord<-metaMDS(comm=Plot,distance="bray",trace=FALSE, k=2,
             autotransform = FALSE,trymax = 100, zerodist = "add")
Plot.ord # k2=0.0767 ~8%  # k3=.058 ~5.8%   #k4=.046 ~4.6%   #k5=.0367 ~3.7%  
stressplot(Plot.ord)


Plot2.ord<-metaMDS(comm=Plot2,distance="bray",trace=FALSE, k=2,
             autotransform = FALSE,trymax = 100, zerodist = "add")
Plot2.ord # k2=0.0768 ~8%  # k3=.0586 ~6%   #k4=.0467 ~4.7%   #k5=.037 ~3.7%  
stressplot(Plot2.ord)






fit.sage <- envfit(ord, sage.A,perm=1000)
fit.sage

fit.sage.H1 <- envfit(H1, sage.A,perm=1000)
fit.sage.H1

fit.sage.H2<- envfit(H2.ord, sage.H2,perm=1000)
fit.sage.H2

fit.sage.Plot <- envfit(Plot.ord, sage.A,perm=1000)
fit.sage.Plot

fit.sage.Plot2 <- envfit(Plot2.ord, sage.A,perm=1000)
fit.sage.Plot2



variableScores <- ord$species # Soil variable scores
sampleScores <- ord$points # Plot scores


# s3d <- scatterplot3d(variableScores)
# text(variableScores,labels=row.names(variableScores))

#plotMDS
orditkplot(Plot.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
orditkplot(Plot.ord, display="species",choices=c(2,3), col="black", cex=0.7, pcol="gray",pch="+",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
orditkplot(Plot.ord, display="species",choices=c(1,3), col="black", cex=0.7, pcol="gray",pch="+",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
# saveRDS(a12, file="F:/SageNMDSvariables/a12.Rdata")
# saveRDS(a23, file="F:/SageNMDSvariables/a23.Rdata")
# saveRDS(a13, file="F:/SageNMDSvariables/a13.Rdata")
# saveRDS(bray, file="F:/SageNMDSvariables/bray.Rdata")
# a12 <- readRDS("F:/SageNMDSvariables/a12.Rdata")
# a23 <- readRDS("F:/SageNMDSvariables/a23.Rdata")
# a13 <- readRDS("F:/SageNMDSvariables/a13.Rdata")

plot(plot12)
title(main = "NMDS 1&2")
plot(fit.sage.Plot,col="blue", cex=0.9,font=2) 

plot(plot23)
title(main = "NMDS 2&3")
plot(fit.sage.Plot,col="blue", cex=0.9,font=2) 

plot(plot13)
title(main = "NMDS 1&3")
plot(fit.sage,col="blue", cex=0.9,font=2) 


ordiplot(ord, type ="n",main="NMDS 1&2", choices=c(1,2))
 text(ord, display="species", col="black", cex=0.7)
 plot(fit.sage,col="blue", cex=0.7)

ordiplot(ord, type ="n",main="NMDS 2&3", choices=c(2,3))
 text(ord, display="species", col="black", cex=0.7)
 plot(fit.sage,col="blue", cex=0.7)

ordiplot(ord, type ="n",main="NMDS 1&3", choices=c(1,3))
 text(ord, display="species", col="black", cex=0.7)
 plot(fit.sage,col="blue", cex=0.7)

##########
# Sagebrush Height Classes

rel.l.total <- read.csv("F:/ShrubDensity/HeightClass/LiveSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
rel.d.total <- read.csv("F:/ShrubDensity/HeightClass/DeadSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
rel.l.d.total <- read.csv("F:/ShrubDensity/HeightClass/LiveDeadSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
den.l <- read.csv("F:/ShrubDensity/HeightClass/LiveDensityM2Class.csv", header=TRUE, row.names=1)
den.d <- read.csv("F:/ShrubDensity/HeightClass/DeadDensityM2Class.csv", header=TRUE, row.names=1)
den.l.d <- read.csv("F:/ShrubDensity/HeightClass/LiveDeadDensityM2Class.csv", header=TRUE, row.names=1)

rel.l.total[is.na(rel.l.total)] <- 0 # replace NA with 0
rel.d.total[is.na(rel.d.total)] <- 0 # replace NA with 0
rel.l.d.total[is.na(rel.l.d.total)] <- 0 # replace NA with 0

fit.rlt <- envfit(ord,rel.l.total,perm=1000)
fit.rdt <- envfit(ord,rel.d.total,perm=1000)
fit.rldt <- envfit(ord,rel.l.d.total,perm=1000)
fit.den.l <- envfit(ord,den.l,perm=1000)
fit.den.d <- envfit(ord,den.d,perm=1000)
fit.den.l.d <- envfit(ord,den.l.d,perm=1000)

fit.rlt
fit.rdt
fit.rldt
fit.den.l
fit.den.d
fit.den.l.d

plot(a12)
title(main = "NMDS Relative Cover 1&2")
plot(fit.rldt,col="blue", cex=0.9,font=2)
plot(fit.rdt,col="red", cex=0.9,font=2)
plot(fit.rlt,col="chartreuse4", cex=0.9,font=2)

plot(a12)
title(main = "NMDS Density1&2")
plot(fit.den.l.d,col="blue", cex=0.9,font=2)
plot(fit.den.d,col="red", cex=0.9,font=2)
plot(fit.den.l,col="chartreuse4", cex=0.9,font=2)


plot(a23)
title(main = "NMDS Relative Cover 2&3")
plot(fit.rldt,col="blue", cex=0.9,font=2)
plot(fit.rdt,col="red", cex=0.9,font=2)
plot(fit.rlt,col="chartreuse4", cex=0.9,font=2)

plot(a23)
title(main = "NMDS Density 2&3")
plot(fit.den.l.d,col="blue", cex=0.9,font=2)
plot(fit.den.d,col="red", cex=0.9,font=2)
plot(fit.den.l,col="chartreuse4", cex=0.9,font=2)


plot(a13)
title(main = "NMDS Relative Cover 1&3")
plot(fit.rldt,col="blue", cex=0.9,font=2)
plot(fit.rdt,col="red", cex=0.9,font=2)
plot(fit.rlt,col="chartreuse4", cex=0.9,font=2)

plot(a13)
title(main = "NMDS Density 1&3")
plot(fit.den.l.d,col="blue", cex=0.9,font=2)
plot(fit.den.d,col="red", cex=0.9,font=2)
plot(fit.den.l,col="chartreuse4", cex=0.9,font=2)
