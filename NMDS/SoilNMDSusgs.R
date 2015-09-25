# Soil NMDS with USGS data added to April data
data.env.NS <- read.csv("F:/Soils/SoilEnvironmentalDataNSplain.csv",header=TRUE, row.names=1)
data.env.U <- read.csv("F:/Soils/SoilEnvironmentalDataUSGSApril.csv",header=TRUE, row.names=1)


data.env.NS[is.na(data.env.NS)] <- 0 # replace NA with 0
data.env.U[is.na(data.env.U)] <- 0 # replace NA with 0


sage.NS <- read.csv("F:/SageNMDSvariables/Sage.Env.NSplainApril.csv",header=TRUE,row.names=1)
sage.U <- read.csv("F:/SageNMDSvariables/Sage.Env.USGS.csv",header=TRUE,row.names=1)

sage.NS[is.na(sage.NS)] <- 0 # replace NA with 0
sage.U[is.na(sage.U)] <- 0 # replace NA with 0

sage.H2 <- sage.U[-c(30,32,38,45,46,69,103),] #remove H2 empty rows.


library(vegan)
library(MASS)
library(colorspace)

#Stress <0.10 indicates that the ordination is good "with no real 
#risk of drawing false inferences" (Clarke 1993, p. 26). 
# linear fit is the fit between ordination values and distances


#
# All Soil Variables
#
All <- data.env.U[,c(1:40,42:43)] # remove SlopeShape (categorical)
All.ord<-metaMDS(comm=All,distance="euc",trace=FALSE, k=2,
                 autotransform = FALSE,trymax = 100, zerodist = "add")
All.ord # k2=0.088 ~9%
stressplot(All.ord)
fit.sage.U <- envfit(All.ord, sage.U,perm=1000)
fit.sage.U    #Sig = Den (l&ld), rel cov(l&ld)
#orditkplot(All.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(uall, file="F:/SageNMDSvariables/uall.Rdata")
# uall <- readRDS("F:/SageNMDSvariables/uall.Rdata")
plot(uall)
title(main = "NMDS All USGS Soil Variables")
plot(fit.sage.U,col="blue", cex=0.9,font=2) 
# Shis shows 3 distinct sets, H1, H2, and Plot


#
# Surface Horizon Variables
#
H1 <- All[,c(1:11)]
H1.ord<-metaMDS(comm=H1,distance="euc",trace=FALSE, k=2,
                autotransform = FALSE,trymax = 100, zerodist = "add")
H1.ord # k2=0.031 ~3%
stressplot(H1.ord)
fit.sage.H1 <- envfit(H1.ord, sage.U,perm=1000)
fit.sage.H1    #Sig = Den (l&ld), prop ind, rel cov(l)
#orditkplot(H1.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(uh1, file="F:/SageNMDSvariables/uh1.Rdata")
# uh1 <- readRDS("F:/SageNMDSvariables/uh1.Rdata")
plot(uh1)
title(main = "NMDS USGS Surface Horizon Soil Variables")
plot(fit.sage.H1,col="blue", cex=0.9,font=2) 

#
# Subsurface Horizon Variables
#
H2 <- All[-c(30,32,38,45,46,69,103),c(12:29)] #H2 data, remove rows with no data
H2.ord<-metaMDS(comm=H2,distance="euc",trace=FALSE, k=2,
                autotransform = FALSE,trymax = 100, zerodist = "ignore")
H2.ord # k2=0.0595 ~6%
stressplot(H2.ord)
fit.sage.H2<- envfit(H2.ord, sage.H2,perm=1000)
fit.sage.H2   # Sig = None
#orditkplot(H2.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(uh2, file="F:/SageNMDSvariables/uh2.Rdata")
# uh2 <- readRDS("F:/SageNMDSvariables/uh2.Rdata")
plot(uh2)
title(main = "NMDS USGS Subsurface Horizon Soil Variables")
plot(fit.sage.H2,col="blue", cex=0.9,font=2) 
# break into Max color, Min color, and all else


#
# Color
#
C <- H2[,c(7:18)] # H2 data Max values
C.ord<-metaMDS(comm=C,distance="euc",trace=FALSE, k=2,
                 autotransform = FALSE,trymax = 100, zerodist = "ignore")
C.ord # k2=0.055 ~6%
stressplot(C.ord)
fit.sage.max<- envfit(H2.ord, sage.H2,perm=1000)
fit.sage.max   # Sig = None
#orditkplot(C.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(max, file="F:/SageNMDSvariables/max.Rdata")
# max <- readRDS("F:/SageNMDSvariables/max.Rdata")
plot(max)
title(main = "NMDS 1&2 Subsurface Horizon Color Soil Variables")
plot(fit.sage.max,col="blue", cex=0.9,font=2) 





#
# Max Color
#
Max <- H2[,c(7,9,11,13,15,17)] # H2 data Max values
Max.ord<-metaMDS(comm=Max,distance="euc",trace=FALSE, k=2,
                 autotransform = FALSE,trymax = 100, zerodist = "ignore")
Max.ord # k2=0.055 ~6%
stressplot(Max.ord)
fit.sage.max<- envfit(H2.ord, sage.H2,perm=1000)
fit.sage.max   # Sig = None
#orditkplot(Max.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(max, file="F:/SageNMDSvariables/max.Rdata")
# max <- readRDS("F:/SageNMDSvariables/max.Rdata")
plot(max)
title(main = "NMDS 1&2 Subsurface Horizon Maximum Color Soil Variables")
plot(fit.sage.max,col="blue", cex=0.9,font=2) 
#orditkplot(Max.ord, display="species",choices=c(2,3), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(max23, file="F:/SageNMDSvariables/max23.Rdata")
# max23 <- readRDS("F:/SageNMDSvariables/max23.Rdata")
plot(max23)
title(main = "NMDS 2&3 Subsurface Horizon Maximum Color Soil Variables")
plot(fit.sage.max,col="blue", cex=0.9,font=2) 
#orditkplot(Max.ord, display="species",choices=c(1,3), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(max13, file="F:/SageNMDSvariables/max13.Rdata")
# max13 <- readRDS("F:/SageNMDSvariables/max13.Rdata")
plot(max13)
title(main = "NMDS 1&3 Subsurface Horizon Maximum Color Soil Variables")
plot(fit.sage.max,col="blue", cex=0.9,font=2) 


#
# Min Color
#
Min <- H2[,c(8,10,12,14,16,18)] # H2 data Min values
Min.ord<-metaMDS(comm=Min,distance="euc",trace=FALSE, k=2,
                 autotransform = FALSE,trymax = 100, zerodist = "ignore")
Min.ord # k2=0.053 ~5%
stressplot(Min.ord)
fit.sage.min<- envfit(Min.ord, sage.H2,perm=1000)
fit.sage.min   # Sig = Sig = Den (l&ld), prop ind, rel cov(l)
#orditkplot(Min.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(min, file="F:/SageNMDSvariables/min.Rdata")
# min <- readRDS("F:/SageNMDSvariables/min.Rdata")
plot(min)
title(main = "NMDS 1&2 Subsurface Horizon Minimum Color Soil Variables")
plot(fit.sage.min,col="blue", cex=0.9,font=2) 
#orditkplot(Min.ord, display="species",choices=c(2,3), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(min23, file="F:/SageNMDSvariables/min23.Rdata")
# min23 <- readRDS("F:/SageNMDSvariables/min23.Rdata")
plot(min23)
title(main = "NMDS 2&3 Subsurface Horizon Minimum Color Soil Variables")
plot(fit.sage.min,col="blue", cex=0.9,font=2) 
#orditkplot(Min.ord, display="species",choices=c(1,3), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(min13, file="F:/SageNMDSvariables/min13.Rdata")
# min13 <- readRDS("F:/SageNMDSvariables/min13.Rdata")
plot(min13)
title(main = "NMDS 1&3 Subsurface Horizon Minimum Color Soil Variables")
plot(fit.sage.min,col="blue", cex=0.9,font=2) 

#
# H2 Subset
#
H2.sub <- H2[,c(1,2,3,4,5,6)] # H2 data Min values
H2.sub.ord<-metaMDS(comm=H2.sub,distance="euc",trace=FALSE, k=2,
                    autotransform = FALSE,trymax = 100, zerodist = "ignore")
H2.sub.ord # k2=0.057 ~6%
stressplot(H2.sub.ord)
fit.sage.H2.sub<- envfit(H2.sub.ord, sage.H2,perm=1000)
fit.sage.H2.sub   # Sig = None
#orditkplot(H2.sub.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(uh2sub, file="F:/SageNMDSvariables/uh2sub.Rdata")
# uh2sub <- readRDS("F:/SageNMDSvariables/uh2sub.Rdata")
plot(uh2sub)
title(main = "NMDS UGSG Subsurface Horizon Subset Soil Variables")
plot(fit.sage.H2.sub,col="blue", cex=0.9,font=2) 



#
# Plot Soil Variables
#
Plot <- All[,c(30:42)]
Plot.ord<-metaMDS(comm=Plot,distance="euc",trace=FALSE, k=2,
                  autotransform = FALSE,trymax = 100, zerodist = "add")
Plot.ord # k2=0.008 ~0.8%  
stressplot(Plot.ord)
fit.sage.Plot <- envfit(Plot.ord, sage.U,perm=1000)
fit.sage.Plot    # Sig = Den (l&ld), rel cov(l&ld)
#orditkplot(Plot.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(uplot, file="F:/SageNMDSvariables/uplot.Rdata")
# uplot <- readRDS("F:/SageNMDSvariables/uplot.Rdata")
plot(uplot)
title(main = "NMDS USGS Plot Level Soil Variables")
plot(fit.sage.Plot,col="blue", cex=0.9,font=2) 





























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

# soil.s <- All.ord$species # Soil variable scores
# plot.s <- All.ord$points # Plot scores
# 
# soil.s <- as.data.frame(soil.s)
# soil.s$soil< - rownames(soil.s)
# soil.s <- cbind(Row.Names = rownames(soil.s), soil.s)
# rownames(soil.s)<-NULL
# 
# library(rgl)
# with(soil.s,plot3d(MDS1,MDS2,MDS3))
# with(soil.s,text3d(x=MDS1,y=MDS2,z=MDS3,text=Row.Names))

# s3d <- scatterplot3d(soil.s)
# text(soil.s,labels=row.names(soil.s))
