#install.packages("vegan")
library(vegan)
library(MASS)
library(colorspace)
#pal <- choose_palette()

#---NMDS Script for Beef Basin Data --------
# this code has been modified from Plant Community Ecology at USU and various online sources
#### Took the file F:/LPI/LPIRelativeCover.csv and used
#### SUM and COUNTIF in Excel to find the column sums and 
#### number of sites each veg was found at then I manually
#### removed those with COUNTIF below 5 AND SUMS below .05 (had to both be below to be removed)
#### Also cleaned up the data (combined SALS0 and SALSO, etc...)

data<-read.csv("F:/LPI/AprilLPIRelativeCoverCommonInExcel.csv",header=TRUE, row.names=1)
###data<-data[,2:ncol(data)] 

# # calculate dissimilarities, use function "vegdist"in VEGAN package
# data.dis<-vegdist(data,method="euc")
# dis.matrix<-as.matrix(data.dis)
# # rankindex compares euclidean, bray-curtis, etc... for my data
# rankindex(dis.matrix,data)

e.data.mds<-metaMDS(comm=data,distance="euc",trace=FALSE)
# m.data.mds<-metaMDS(comm=data,distance="man",trace=FALSE)
# g.data.mds<-metaMDS(comm=data,distance="gow",trace=FALSE)
# b.data.mds<-metaMDS(comm=data,distance="bray",trace=FALSE)
# k.data.mds<-metaMDS(comm=data,distance="kul",trace=FALSE)

e.data.mds #.09
# m.data.mds #.13
# g.data.mds #.20
# b.data.mds #.13
# k.data.mds #.14

plot.sc = scores(e.data.mds)
# Stress <0.10 indicates that the ordination is good "with no real 
# risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances
stressplot(e.data.mds) 

###########################  ENVIRONMENTAL DATA  ###########################################

# read in Soil environmental data
data.env <- read.csv("F:/Soils/SoilEnvironmentalDataModWithColbyAWS.csv",header=TRUE, row.names=1)

# read in SageEnvironmentalNMDS data
data.sage <- read.csv("F:/SageNMDSvariables/Sage.Env.April.csv",header=TRUE,row.names=1)
data.sage[is.na(data.sage)] <- 0 # replace NA with 0


# read in Artr live and dead as environmental data
data.l <- read.csv("F:/ShrubDensity/HeightClass/LivePlotbySizeClass.csv", header=TRUE, row.names=1)
data.d <- read.csv("F:/ShrubDensity/HeightClass/DeadPlotbySizeClass.csv", header=TRUE, row.names=1)
data.l.d <-read.csv("F:/ShrubDensity/HeightClass/LiveDeadPlotbySizeClass.csv", header=TRUE, row.names=1) 
prop.l.sage <- read.csv("F:/ShrubDensity/HeightClass/LiveSizeClassSagePctCover.csv", header=TRUE, row.names=1)
# rel.d.sage <- read.csv("F:/ShrubDensity/HeightClass/DeadSizeClassSagePctCover.csv", header=TRUE, row.names=1)
# rel.l.d.sage <- read.csv("F:/ShrubDensity/HeightClass/LiveDeadSizeClassSagePctCover.csv", header=TRUE, row.names=1)
rel.l.total <- read.csv("F:/ShrubDensity/HeightClass/LiveSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
rel.d.total <- read.csv("F:/ShrubDensity/HeightClass/DeadSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
rel.l.d.total <- read.csv("F:/ShrubDensity/HeightClass/LiveDeadSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
den.l <- read.csv("F:/ShrubDensity/HeightClass/LiveDensityM2Class.csv", header=TRUE, row.names=1)
den.d <- read.csv("F:/ShrubDensity/HeightClass/DeadDensityM2Class.csv", header=TRUE, row.names=1)
den.l.d <- read.csv("F:/ShrubDensity/HeightClass/LiveDeadDensityM2Class.csv", header=TRUE, row.names=1)

prop.l.sage[is.na(prop.l.sage)] <- 0 # replace NA with 0
# rel.d.sage[is.na(rel.d.sage)] <- 0 # replace NA with 0
# rel.l.d.sage[is.na(rel.l.d.sage)] <- 0 # replace NA with 0
rel.l.total[is.na(rel.l.total)] <- 0 # replace NA with 0
rel.d.total[is.na(rel.d.total)] <- 0 # replace NA with 0
rel.l.d.total[is.na(rel.l.d.total)] <- 0 # replace NA with 0

#function "envfit" fits environmental vectors or factors onto an ordination.
#requires ordination plot first before plot(fit)
fit.env <- envfit(e.data.mds,data.env,perm=1000)
fit.sage <- envfit(e.data.mds, data.sage,perm=1000)
fit.l <- envfit(e.data.mds,data.l,perm=1000)
fit.d <- envfit(e.data.mds,data.d,perm=1000)
fit.l.d <- envfit(e.data.mds,data.l.d,perm=1000)
fit.pls <- envfit(e.data.mds,prop.l.sage,perm=1000)
# fit.rds <- envfit(e.data.mds,rel.d.sage,perm=1000)
# fit.rlds <- envfit(e.data.mds,rel.l.d.sage,perm=1000)
fit.rlt <- envfit(e.data.mds,rel.l.total,perm=1000)
fit.rdt <- envfit(e.data.mds,rel.d.total,perm=1000)
fit.rldt <- envfit(e.data.mds,rel.l.d.total,perm=1000)
fit.den.l <- envfit(e.data.mds,den.l.total,perm=1000)
fit.den.d <- envfit(e.data.mds,den.d.total,perm=1000)
fit.den.l.d <- envfit(e.data.mds,den.l.d.total,perm=1000)

fit.env
fit.sage
fit.l
fit.d
fit.l.d
fit.pls
# fit.rds
# fit.rlds
fit.rlt
fit.rdt
fit.rldt
fit.den.l
fit.den.l.d

### Choose only the significant environmental data
sig.data.env<-data.env[,c(8,9,12,16:18,24)]
sig.fit.env<-envfit(e.data.mds,sig.data.env,perm=1000)
sig.fit.env # Check that you pulled up the right factors.


#plotMDS
orditkplot(e.data.mds, display="species", col="black", cex=0.7, pcol="gray",pch="+",xlim=c(-0.7,0.7),ylim=c(-0.7,0.4))
saveRDS(Plot, file="F:/SageNMDSvariables/Plot.Rdata")
Plot <- readRDS("F:/SageNMDSvariables/Plot.Rdata")

plot(Plot)
title(main = "NMDS and Soils")
plot(sig.fit.env,col="blue", cex=0.7,font=2)

plot(Plot)
title(main = "NMDS and Sagebrush")
plot(fit.sage,col="chartreuse4", cex=0.7,font=2)

plot(Plot)
title(main = "NMDS and Live Classes")
plot(fit.l,col="blue", cex=0.7,font=2)
plot(fit.pls,col="dodgerblue", cex=0.7,font=2)
plot(fit.rlt,col="blue4", cex=0.7,font=2)
plot(fit.den.l,col="blue4", cex=0.7,font=2)

plot(Plot)
title(main = "NMDS and Dead Classes")
plot(fit.d,col="blue", cex=0.7,font=2)
# plot(fit.rds,col="orange", cex=0.7,font=2)
plot(fit.rdt,col="pink4", cex=0.7,font=2)
plot(fit.den.d,col="blue4", cex=0.7,font=2)

plot(Plot)
title(main = "NMDS and Live+Dead Classes")
plot(fit.l.d,col="blue", cex=0.7,font=2)
# plot(fit.rlds,col="chartreuse4", cex=0.7,font=2)
plot(fit.rldt,col="blue", cex=0.7,font=2)
plot(fit.den.l.d,col="blue4", cex=0.7,font=2)

#ordiplot(data.mds)
# ordiplot(e.data.mds, display ="species", type ="n")
# text(e.data.mds, display="sites", col="black", cex=0.7)
# text(e.data.mds, display="species", col="red", cex=0.7)
# 
# ordiplot(e.data.mds, type ="n",main="NMDS Soil Data")
# text(e.data.mds, display="species", col="black", cex=0.7)
# plot(sig.fit.env,col="blue", cex=0.7)

#plot environmental loadings
plot(e.data.mds,type="t",main="NMDS Soil Data")
  plot(sig.fit.env,col="blue", cex=0.7)
plot(e.data.mds,type="t",main="NMDS Sage Data")
  plot(fit.sage,col="chartreuse4", cex=0.7)
plot(e.data.mds,type="t",main="NMDS ARTR.L Data")
  plot(fit.l,col="green3", cex=0.7)
plot(e.data.mds,type="t",main="NMDS ARTR.D Data")
  plot(fit.d,col="purple", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.L/All ARTR Data")
  plot(fit.rls,col="dodgerblue", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.D/All ARTR Data")
  plot(fit.rds,col="orange", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.L/All Shrubs Data")
  plot(fit.rlt,col="darkmagenta", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.D/All Shrubs Data")
  plot(fit.rdt,col="blueviolet", cex=0.7)
