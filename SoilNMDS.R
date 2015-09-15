library(vegan)
library(MASS)
library(colorspace)

# read in data
data.env <- read.csv("F:/Soils/SoilEnvironmentalDataModWithColbyAWS.csv",header=TRUE, row.names=1)

# add shrub environmental variables
sage <- read.csv("F:/SageNMDSvariables/Sage.Env.April.csv",header=TRUE,row.names=1)
sage[is.na(sage)] <- 0 # replace NA with 0

# set factors to factor not numeric
ix <- c(2,10:25)
data.env[ix] <- lapply(data.env[ix], as.factor) 
id <- c(1,3:9,26:30)
data.env[id] <- lapply(data.env[id], as.numeric)
data <- data.env[,c(1,3:9,26:30)]

# look at distance methods
rankindex(sage, data, c("euc","man","bray", "jac", "kul"))

# run metaMDS
ord <- metaMDS(comm=data,distance="bray",trace=FALSE)
ord # 0.156 ~16%
stressplot(ord) 

fit.sage <- envfit(ord, sage,perm=1000)
fit.sage

#plotMDS
orditkplot(ord, display="species", col="black", cex=0.7, pcol="gray",pch="+")
saveRDS(bray, file="F:/SageNMDSvariables/bray.Rdata")
# Plot <- readRDS("F:/SageNMDSvariables/Plot.Rdata")

plot(bray)
title(main = "NMDS Bray-Curtis")
plot(fit.sage,col="blue", cex=0.9,font=2)



,xlim=c(-0.20,0.20),ylim=c(-0.1,0.1)







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


plot(bray)
title(main = "NMDS Bray-Curtis")
plot(fit.sage,col="blue", cex=0.9,font=2)
