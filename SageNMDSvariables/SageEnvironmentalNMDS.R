# Do oridnation with soil properties, then put on the veg vecotrs to see
# how they relate to the properties.

# veg vectors:
# 1-3   Sage density (m2) (Shrub)live, dead, live+dead
# 4-9   Sage relative cover (Shrub then LPI) live, dead, live+dead
# 10    Proportion Sage individuals alive to dead (m2) (Shrub)
# 11    Proportion Sage cover alive (LPI)
# 12    Perrenial Grass ralative cover (LPI)

library(plyr)

# Read in Shrub data
class <- read.csv('F:/ShrubDensity/HeightClass/PlantDenDetail 8-21.csv')
class$total <- rowSums(class[,4:8]) #Sum all size classes for total density
total <- class[, c(1,2,3,9)] # Just total, size class removed

    # Shrub Totals (plot by species)
    TotalplotXspp<-xtabs(total~Plot+SpeciesCode, total) # put in plot by spp matrix
    write.csv(TotalplotXspp,file="F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv")
    Total <- read.csv("F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv",row.names=1)
    
    # Shrub Density (plot by species)
    TotaldensityM2 <- TotalplotXspp/180 # density per M2
    write.csv(TotaldensityM2,file="F:/ShrubDensity/HeightClass/USGSTotalDensityM2.csv")
    TotalDen <- read.csv("F:/ShrubDensity/HeightClass/USGSTotalDensityM2.csv",row.names=1)
    TotalDen <- TotalDen[,c(8:9)]

# Read in LPI data
lpi <- read.csv("F:/LPI/USGSLPIRelativeCoverCommonInExcel.csv")
lpi <- lpi[-c(160:162),] # remove SUMS and COUNTIF rows at the bottom
row.names(lpi)<-lpi$X
lpi <- lpi[,-1] # remove extra plot id column


# Sage density
# know area sampled: 30m transects X 2m belt X 3 transects per plot 
# (2,3,& 4) = 180m2. Density in m2
# Total number of a spp. in a plot divided by the number of meters sampled in a plot(180m for shrub transects)
# Then choose only ARTR2
TotalDen$L.DenM2.S <- TotalDen$ARTR2
TotalDen$D.DenM2.S <- TotalDen$ARTR2.D
TotalDen$L.D.DenM2.S <- (TotalDen$ARTR2 + TotalDen$ARTR2.D)
Sage.Env.USGS <- TotalDen[,c(3:5)]

# Sage relative cover using Shrub data
# add all points of a spp. in a plot, then sum ARTR2 and ARTR2.D. Next divide by the total number of individuals of all species.
Sage.Env.USGS$L.RelCov.S <- ((Total$ARTR2)/rowSums(Total))*100
Sage.Env.USGS$D.RelCov.S <- ((Total$ARTR2.D)/rowSums(Total))*100
Sage.Env.USGS$L.D.RelCov.S <- ((Total$ARTR2 + Total$ARTR2.D)/rowSums(Total))*100

# Sage relative cover using LPI data
Sage.Env.USGS$L.RelCov.LPI <- ((lpi$ARTR2)/rowSums(lpi))*100
Sage.Env.USGS$D.RelCov.LPI <- ((lpi$ARTR2.D)/rowSums(lpi))*100
Sage.Env.USGS$L.D.RelCov.LPI <- ((lpi$ARTR2 + lpi$ARTR2.D)/rowSums(lpi))*100

# Proportion Sage individuals alive to dead.
# (number of live individual sagebrush plants)/(number of live + number of dead individual sagebrush plants)
Sage.Env.USGS$L.PropIndM2.S <- ((Total$ARTR2)/(Total$ARTR2+Total$ARTR2.D))

# Proportion Sage cover alive
# (SageRelativeCover(live))/(SageRelativeCover(live+dead))
Sage.Env.USGS$L.PropCov.LPI <- Sage.Env.USGS$L.RelCov.LPI/Sage.Env.USGS$L.D.RelCov.LPI

# Perrenial Grass relative cover
Sage.Env.USGS$PG.RelCov.LPI <- (lpi$ACHY+lpi$ARPU9+lpi$BOGR2+lpi$BOGR2.D+lpi$ELEL+lpi$HECO26+lpi$HECO26.D+lpi$SPCR+lpi$SPCR.D+lpi$HIJA)

#######################################################################
# Remove USGS, only April Data
Sage.Env.April <- Sage.Env.USGS[-c(1:60),] # remove usgs data
# Write April Data
write.csv(Sage.Env.April,file="F:/SageNMDSvariables/Sage.Env.April.csv")

# Choose only rows that have soils data
VegtoKeep <- Sage.Env.USGS[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),]

# Add April data back in 
Keep <- rbind(Sage.Env.April, VegtoKeep)
write.csv(Keep,file="F:/SageNMDSvariables/Sage.Env.USGS.csv")

####
# Choose 10 USGS plots in N&S plain
NS <- Sage.Env.USGS[c("19","24","33","39","43","44","47","48","50"),]
# ("24","38","40","42","43","80","82")
NSApril <- rbind(Sage.Env.April,NS)
write.csv(NSApril,file="F:/SageNMDSvariables/Sage.Env.NSplainApril.csv")

