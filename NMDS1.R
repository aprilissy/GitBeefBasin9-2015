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
data.dis<-vegdist(data,method="bray")
dis.matrix<-as.matrix(data.dis)
# rankindex compares euclidean, bray-curtis, etc... for my data
rankindex(dis.matrix,data)


data.mds<-metaMDS(comm=data,distance="bray",trace=FALSE)
data.mds
# Stress <0.10 indicates that the ordination is good "with no real 
# risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances
stressplot(data.mds) 

###########################  ENVIRONMENTAL DATA  ###########################################


# read in Soil environmental data
data.env <- read.csv("F:/Soils/SoilEnvironmentalDataModWithColbyAWS.csv",header=TRUE, row.names=1)
###data.env<-data.env[,2:ncol(data.env)]

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
# #add shrub height class densities as environmental data
#   # remove columns with only 0 values where there were no shrubs of that species in that size class found.
# A <- read.csv("F:/ShrubDensity/HeightClass/AprilAdensityM2.csv",row.names=1)
#   A <- A[, colSums(A != 0) > 0]
# B <- read.csv("F:/ShrubDensity/HeightClass/AprilBdensityM2.csv",row.names=1)
#   B <- B[, colSums(B != 0) > 0]
# C <- read.csv("F:/ShrubDensity/HeightClass/AprilCdensityM2.csv",row.names=1)
#   C <- C[, colSums(C != 0) > 0]
# D <- read.csv("F:/ShrubDensity/HeightClass/AprilDdensityM2.csv",row.names=1)
#   D <- D[, colSums(D != 0) > 0]
# E <- read.csv("F:/ShrubDensity/HeightClass/AprilEdensityM2.csv",row.names=1)
#   E <- E[, colSums(E != 0) > 0]

#function "envfit" fits environmental vectors or factors onto an ordination.
#requires ordination plot first before plot(fit)
fit.env <- envfit(data.mds,data.env,perm=1000)
fit.l <- envfit(data.mds,data.l,perm=1000)
fit.d <- envfit(data.mds,data.d,perm=1000)
fit.rls <- envfit(data.mds,rel.l.sage,perm=1000)
fit.rds <- envfit(data.mds,rel.d.sage,perm=1000)
fit.rlt <- envfit(data.mds,rel.l.total,perm=1000)
fit.rdt <- envfit(data.mds,rel.d.total,perm=1000)
# fit.a<-envfit(data.mds,A,perm=1000)
# fit.b<-envfit(data.mds,B,perm=1000)
# fit.c<-envfit(data.mds,C,perm=1000)
# fit.d<-envfit(data.mds,D,perm=1000)
# fit.e<-envfit(data.mds,E,perm=1000)
# Look at the significant factors
fit.env
fit.l
fit.d
fit.rls
fit.rds
fit.rlt
fit.rdt
# fit.a
# fit.b
# fit.c
# fit.d
# fit.e
### Choose only the significant environmental data
sig.data.env<-data.env[,c(1,7:9,19:25,28:29)]
sig.fit.env<-envfit(data.mds,sig.data.env,perm=1000)
sig.fit.env # Check that you pulled up the right factors.

# sig.A<-A[,c(1,3)]
# sig.fit.A<-envfit(data.mds,sig.A,perm=1000)
# sig.fit.A # Check that you pulled up the right factors.
# 
# sig.B<-B[,c(2:3,9)]
# sig.fit.B<-envfit(data.mds,sig.B,perm=1000)
# sig.fit.B # Check that you pulled up the right factors.
# 
# sig.C<-C[,c(1:3,6,8,12:13)]
# sig.fit.C<-envfit(data.mds,sig.C,perm=1000)
# sig.fit.C # Check that you pulled up the right factors.
# 
# sig.D<-D[,c(3,8,11,13)]
# sig.fit.D<-envfit(data.mds,sig.D,perm=1000)
# sig.fit.D # Check that you pulled up the right factors.
# 
# sig.E<-E[,c(3,7:8)]
# sig.fit.E<-envfit(data.mds,sig.E,perm=1000)
# sig.fit.E # Check that you pulled up the right factors.


#plotMDS

#ordiplot(data.mds)
ordiplot(data.mds, display ="species", type ="n")
text(data.mds, display="sites", col="black", cex=0.7)
text(data.mds, display="species", col="red", cex=0.7)

#plot environmental loadings
plot(data.mds,type="t",main="NMDS Soil Data")
  plot(sig.fit.env,col="blue", cex=0.7)
plot(data.mds,type="t",main="NMDS ARTR.L Data")
  plot(fit.l,col="green", cex=0.7)
plot(data.mds,type="t",main="NMDS ARTR.D Data")
  plot(fit.d,col="purple", cex=0.7)
plot(data.mds,type="t",main="NMDS % ARTR.L/All ARTR Data")
  plot(fit.rls,col="yellow", cex=0.7)
plot(data.mds,type="t",main="NMDS % ARTR.D/All ARTR Data")
  plot(fit.rds,col="orange", cex=0.7)
plot(data.mds,type="t",main="NMDS % ARTR.L/All Shrubs Data")
  plot(fit.rlt,col="brown", cex=0.7)
plot(data.mds,type="t",main="NMDS % ARTR.D/All Shrubs Data")
  plot(fit.rdt,col="sienna", cex=0.7)

# plot(data.mds,type="t",main="NMDS using Euclidean Distance, Shrubs <15cm Juvenile ")
#   plot(sig.fit.A,col="orange", cex=0.7)
# plot(data.mds,type="t",main="NMDS using Euclidean Distance, Shrubs <15cm Mature")
#   plot(sig.fit.B,col="purple", cex=0.7)
# plot(data.mds,type="t",main="NMDS using Euclidean Distance, Shrubs 15-50cm")
#   plot(sig.fit.C,col="green", cex=0.7)
# plot(data.mds,type="t",main="NMDS using Euclidean Distance, Shrubs >50c-100cm")
#   plot(sig.fit.D,col="turquoi3se2", cex=0.7)
# plot(data.mds,type="t",main="NMDS using Euclidean Distance, Shrubs >100cm")
#   plot(sig.fit.E,col="hotpink", cex=0.7)
