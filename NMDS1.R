#install.packages("vegan")
library(vegan)
library(MASS)
library(colorspace)
# pal <- choose_palette()

#---NMDS Script for Beef Basin Data --------
# this code has been modified from Plant Community Ecology at USU and various online sources
#### Took the file F:/LPI/LPIRelativeCover.csv and used
#### SUM and COUNTIF in Excel to find the column sums and 
#### number of sites each veg was found at then I manually
#### removed those with COUNTIF below 5 AND SUMS below .05 (had to both be below to be removed)
#### Also cleaned up the data (combined SALS0 and SALSO, etc...)

data<-read.csv("F:/LPI/AprilLPIRelativeCoverCommonInExcel.csv",header=TRUE, row.names=1)
###data<-data[,2:ncol(data)] 

# calculate dissimilarities, use function "vegdist"in VEGAN package
data.dis<-vegdist(data,method="euc")
dis.matrix<-as.matrix(data.dis)
# rankindex compares euclidean, bray-curtis, etc... for my data
rankindex(dis.matrix,data)

e.data.mds<-metaMDS(comm=data,distance="euc",trace=FALSE)
m.data.mds<-metaMDS(comm=data,distance="man",trace=FALSE)
g.data.mds<-metaMDS(comm=data,distance="gow",trace=FALSE)
b.data.mds<-metaMDS(comm=data,distance="bray",trace=FALSE)
k.data.mds<-metaMDS(comm=data,distance="kul",trace=FALSE)

e.data.mds #.09
m.data.mds #.13
g.data.mds #.20
b.data.mds #.13
k.data.mds #.14


# Stress <0.10 indicates that the ordination is good "with no real 
# risk of drawing false inferences" (Clarke 1993, p. 26). 
# My stress is ~0.13
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
rel.l.sage <- read.csv("F:/ShrubDensity/HeightClass/LiveSizeClassSagePctCover.csv", header=TRUE, row.names=1)
rel.d.sage <- read.csv("F:/ShrubDensity/HeightClass/DeadSizeClassSagePctCover.csv", header=TRUE, row.names=1)
rel.l.total <- read.csv("F:/ShrubDensity/HeightClass/LiveSizeClassTotalPctCover.csv", header=TRUE, row.names=1)
rel.d.total <- read.csv("F:/ShrubDensity/HeightClass/DeadSizeClassTotalPctCover.csv", header=TRUE, row.names=1)

rel.l.sage[is.na(rel.l.sage)] <- 0 # replace NA with 0
rel.d.sage[is.na(rel.d.sage)] <- 0 # replace NA with 0
rel.l.total[is.na(rel.l.total)] <- 0 # replace NA with 0
rel.d.total[is.na(rel.d.total)] <- 0 # replace NA with 0

#function "envfit" fits environmental vectors or factors onto an ordination.
#requires ordination plot first before plot(fit)
fit.env <- envfit(e.data.mds,data.env,perm=1000)
fit.sage <- envfit(e.data.mds, data.sage,perm=1000)
fit.l <- envfit(e.data.mds,data.l,perm=1000)
fit.d <- envfit(e.data.mds,data.d,perm=1000)
fit.rls <- envfit(e.data.mds,rel.l.sage,perm=1000)
fit.rds <- envfit(e.data.mds,rel.d.sage,perm=1000)
fit.rlt <- envfit(e.data.mds,rel.l.total,perm=1000)
fit.rdt <- envfit(e.data.mds,rel.d.total,perm=1000)

fit.env
fit.sage
fit.l
fit.d
fit.rls
fit.rds
fit.rlt
fit.rdt

### Choose only the significant environmental data
sig.data.env<-data.env[,c(8:9,12,16,17,24)]
sig.fit.env<-envfit(e.data.mds,sig.data.env,perm=1000)
sig.fit.env # Check that you pulled up the right factors.

#plotMDS

#ordiplot(data.mds)
ordiplot(e.data.mds, display ="species", type ="n")
text(e.data.mds, display="sites", col="black", cex=0.7)
text(e.data.mds, display="species", col="red", cex=0.7)

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
  plot(fit.rls,col="yellow", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.D/All ARTR Data")
  plot(fit.rds,col="orange", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.L/All Shrubs Data")
  plot(fit.rlt,col="brown", cex=0.7)
plot(e.data.mds,type="t",main="NMDS % ARTR.D/All Shrubs Data")
  plot(fit.rdt,col="sienna", cex=0.7)
