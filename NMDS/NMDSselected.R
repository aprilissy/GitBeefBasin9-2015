# NMDS on Selected Variables
## selected using Boruta and Random Forest Variable Importance Plots.

# read in data
All <- read.csv("F:/Soils/SoilEnvironmentalDataApril.csv",header=TRUE, row.names=1)
All[is.na(All)] <- 0 # replace NA with 0
All <- All[,c("H2_MinDryHue","H2_MinDryChroma","Depth100","Depth150","TotalDepth","AWC25","AWC50","AWC100")]


# add shrub environmental variables
sage.A <- read.csv("F:/SageNMDSvariables/Sage.Env.April.csv",header=TRUE,row.names=1)
sage.A[is.na(sage.A)] <- 0 # replace NA with 0
sage.H2 <- sage.A[-c(30,32,38,45,46,69),] #remove H2 empty rows.


#
# All Soil Variables
#
All.ord<-metaMDS(comm=All,distance="euc",trace=FALSE, k=2,
                 autotransform = FALSE,trymax = 100, zerodist = "add")
All.ord # k2=0.0018 ~0.2%
stressplot(All.ord)
fit.sage.A <- envfit(All.ord, sage.A,perm=1000)
fit.sage.A    #Sig = Den (l&ld), rel cov(l&ld)
#orditkplot(All.ord, display="species",choices=c(1,2), col="black", cex=0.7, pcol="gray",pch="+")
# saveRDS(sel, file="F:/SageNMDSvariables/sel.Rdata")
# sel <- readRDS("F:/SageNMDSvariables/sel.Rdata")
plot(sel)
title(main = "NMDS Selected Soil Variables")
plot(fit.sage.A,col="blue", cex=0.9,font=2) 
# Shis shows 3 distinct sets, H1, H2, and Plot
