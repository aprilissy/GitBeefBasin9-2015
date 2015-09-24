library(vegan)
library(MASS)
library(colorspace)

#Stress <0.10 indicates that the ordination is good "with no real 
#risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances

# read in data
All <- read.csv("F:/Soils/SoilEnvironmentalDataApril.csv",header=TRUE, row.names=1)
All[is.na(All)] <- 0 # replace NA with 0
# add shrub environmental variables
sage.A <- read.csv("F:/SageNMDSvariables/Sage.Env.April.csv",header=TRUE,row.names=1)
sage.A[is.na(sage.A)] <- 0 # replace NA with 0
sage.H2 <- sage.A[-c(30,32,38,45,46,69),] #remove H2 empty rows.

#
# All Soil Variables
#
All <- All[,c(1:40,42:43)] # remove SlopeShape (categorical)
All.ord<-metaMDS(comm=All,distance="euc",trace=FALSE, k=2,
                 autotransform = FALSE,trymax = 100, zerodist = "add")
All.ord # k2=0.0285 ~3%
stressplot(All.ord)
fit.sage.A <- envfit(All.ord, sage.A,perm=1000)
fit.sage.A    #Sig = Den (l&ld), rel cov(l&ld)
#orditkplot(All.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
plot(all)
title(main = "NMDS All Soil Variables")
plot(fit.sage.A,col="blue", cex=0.9,font=2) 
# Shis shows 3 distinct sets, H1, H2, and Plot


#
# Surface Horizon Variables
#
H1 <- All[,c(1:11)]
H1.ord<-metaMDS(comm=H1,distance="euc",trace=FALSE, k=2,
                autotransform = FALSE,trymax = 100, zerodist = "add")
H1.ord # k2=0.0265 ~3%
stressplot(H1.ord)
fit.sage.H1 <- envfit(H1.ord, sage.A,perm=1000)
fit.sage.H1    #Sig = Den (l&ld), prop ind, rel cov(l)
#orditkplot(H1.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
plot(h1)
title(main = "NMDS Surface Horizon Soil Variables")
plot(fit.sage.H1,col="blue", cex=0.9,font=2) 

#
# Subsurface Horizon Variables
#
H2 <- All[-c(30,32,38,45,46,69),c(12:29)] #H2 data, remove rows with no data
H2.ord<-metaMDS(comm=H2,distance="euc",trace=FALSE, k=2,
                autotransform = FALSE,trymax = 100, zerodist = "ignore")
H2.ord # k2=0.066 ~7%
stressplot(H2.ord)
fit.sage.H2<- envfit(H2.ord, sage.H2,perm=1000)
fit.sage.H2   # Sig = None
#orditkplot(H2.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
plot(h2)
title(main = "NMDS Subsurface Horizon Soil Variables")
plot(fit.sage.H2,col="blue", cex=0.9,font=2) 
# break into Max color, Min color, and all else

  #
  # Max Color
  #
      Max <- H2[,c(7,9,11,13,15,17)] # H2 data Max values
      Max.ord<-metaMDS(comm=Max,distance="euc",trace=FALSE, k=3,
                      autotransform = FALSE,trymax = 100, zerodist = "ignore")
      Max.ord # k2=0.055 ~6%
      stressplot(Max.ord)
      fit.sage.max<- envfit(H2.ord, sage.H2,perm=1000)
      fit.sage.max   # Sig = None
      #orditkplot(Max.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
      plot(max)
      title(main = "NMDS 1&2 Subsurface Horizon Maximum Color Soil Variables")
      plot(fit.sage.max,col="blue", cex=0.9,font=2) 
      #orditkplot(Max.ord, display="species",choices=c(2,3), col="black", cex=0.7, pcol="gray",pch="+")
      plot(max23)
      title(main = "NMDS 2&3 Subsurface Horizon Maximum Color Soil Variables")
      plot(fit.sage.max,col="blue", cex=0.9,font=2) 
      #orditkplot(Max.ord, display="species",choices=c(1,3), col="black", cex=0.7, pcol="gray",pch="+")
      plot(max13)
      title(main = "NMDS 1&3 Subsurface Horizon Maximum Color Soil Variables")
      plot(fit.sage.max,col="blue", cex=0.9,font=2) 


  #
  # Min Color
  #
      Min <- H2[,c(8,10,12,14,16,18)] # H2 data Min values
      Min.ord<-metaMDS(comm=Min,distance="euc",trace=FALSE, k=3,
                      autotransform = FALSE,trymax = 100, zerodist = "ignore")
      Min.ord # k2=0.053 ~5%
      stressplot(Min.ord)
      fit.sage.min<- envfit(Min.ord, sage.H2,perm=1000)
      fit.sage.min   # Sig = Sig = Den (l&ld), prop ind, rel cov(l)
      #orditkplot(Min.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
      plot(min)
      title(main = "NMDS 1&2 Subsurface Horizon Minimum Color Soil Variables")
      plot(fit.sage.min,col="blue", cex=0.9,font=2) 
      #orditkplot(Min.ord, display="species",choices=c(2,3), col="black", cex=0.7, pcol="gray",pch="+")
      plot(min23)
      title(main = "NMDS 2&3 Subsurface Horizon Minimum Color Soil Variables")
      plot(fit.sage.min,col="blue", cex=0.9,font=2) 
      #orditkplot(Min.ord, display="species",choices=c(1,3), col="black", cex=0.7, pcol="gray",pch="+")
      plot(min13)
      title(main = "NMDS 1&3 Subsurface Horizon Minimum Color Soil Variables")
      plot(fit.sage.min,col="blue", cex=0.9,font=2) 

  #
  # H2 Subset
  #
      H2.sub <- H2[,c(1,2,3,4,5,6)] # H2 data Min values
      H2.sub.ord<-metaMDS(comm=H2.sub,distance="euc",trace=FALSE, k=2,
                       autotransform = FALSE,trymax = 100, zerodist = "ignore")
      H2.sub.ord # k2=0.063 ~6%
      stressplot(H2.sub.ord)
      fit.sage.H2.sub<- envfit(H2.sub.ord, sage.H2,perm=1000)
      fit.sage.H2.sub   # Sig = None
      #orditkplot(H2.sub.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
      plot(h2sub)
      title(main = "NMDS Subsurface Horizon Subset Soil Variables")
      plot(fit.sage.H2.sub,col="blue", cex=0.9,font=2) 



#
# Plot Soil Variables
#
Plot <- All[,c(30:42)]
Plot.ord<-metaMDS(comm=Plot,distance="euc",trace=FALSE, k=2,
                  autotransform = FALSE,trymax = 100, zerodist = "add")
Plot.ord # k2=0.008 ~0.8%  
stressplot(Plot.ord)
fit.sage.Plot <- envfit(Plot.ord, sage.A,perm=1000)
fit.sage.Plot    # Sig = Den (l&ld), rel cov(l&ld)
#orditkplot(Plot.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
plot(plot)
title(main = "NMDS Plot Level Soil Variables")
plot(fit.sage.Plot,col="blue", cex=0.9,font=2) 




#plotMDS
# saveRDS(bray, file="F:/SageNMDSvariables/bray.Rdata")
# a12 <- readRDS("F:/SageNMDSvariables/a12.Rdata")



ordiplot(All.ord, type ="n",main="NMDS 1&2", choices=c(1,2))
 text(All.ord, display="species", col="black", cex=0.7)
 plot(fit.sage.A,col="blue", cex=0.7)

ordiplot(All.ord, type ="n",main="NMDS 2&3", choices=c(2,3))
 text(All.ord, display="species", col="black", cex=0.7)
 plot(fit.sage.A,col="blue", cex=0.7)

ordiplot(All.ord, type ="n",main="NMDS 1&3", choices=c(1,3))
 text(All.ord, display="species", col="black", cex=0.7)
 plot(fit.sage.A,col="blue", cex=0.7)

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
