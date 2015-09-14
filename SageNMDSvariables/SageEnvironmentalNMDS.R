# Do oridnation with soil properties, then put on the veg vecotrs to see
# how they relate to the properties.

# veg vectors:
# 1   Sage density (live+dead)
# 2   Sage density (live)
# 3   Proportion Sage individuals alive (talk to Susan about how to deal with sage absences)
# 4   Proportion Sage cover alive
# 5   Sage relative cover (live+dead)
# 6   Sage relative cover (live)
# 7   Perrenial Grass ralative cover

library(plyr)

# 1
# Sage density (live+dead)
# know area sampled: 30m transects X 2m belt X 3 transects per plot 
# (2,3,& 4) = 180m2. Density in m2
# Total number of a spp. in a plot divided by the number of meters sampled in a plot(180m for shrub transects)
# Then add ARTR2 and ARTR2.D

class <- read.csv('F:/ShrubDensity/HeightClass/PlantDenDetail 8-21.csv')
class$total <- rowSums(class[,4:8]) #Sum all size classes for total density
total <- class[, c(1,2,3,9)] # Just total, size class removed

TotalplotXspp<-xtabs(total~Plot+SpeciesCode, total) # put in plot by spp matrix
TotaldensityM2 <- TotalplotXspp/180 # density per M2
write.csv(TotaldensityM2,file="F:/ShrubDensity/HeightClass/USGSTotalDensityM2.csv")
TotalDen <- read.csv("F:/ShrubDensity/HeightClass/USGSTotalDensityM2.csv",row.names=1)
TotalDen <- TotalDen[,c(8:9)]
TotalDen$L.D.DenM2 <- (TotalDen$ARTR2 + TotalDen$ARTR2.D)

# 2
# Sage density (live)
# Total number of a spp. in a plot divided by the number of meters sampled in a plot(180m for shrub transects)
# Then choose only ARTR2

TotalDen$L.DenM2 <- TotalDen$ARTR2
Sage.Env.USGS <- TotalDen[,c(3:4)]

# 3 
# Proportion Sage individuals alive (talk to Susan about how to deal with sage absences)
# (number of live individual sagebrush plants)/(number of live + number of dead individual sagebrush plants)

TotalplotXspp<-xtabs(total~Plot+SpeciesCode, total) # put in plot by spp matrix
write.csv(TotalplotXspp,file="F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv")
Total <- read.csv("F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv",row.names=1)

Sage.Env.USGS$L.PropInd <- ((Total$ARTR2)/(Total$ARTR2+Total$ARTR2.D))


# 4 
# Proportion Sage cover alive
# (SageRelativeCover(live))/(SageRelativeCover(live+dead))

# Code for #5
Sage.Env.USGS$L.RelCov <- ((Total$ARTR2)/rowSums(Total))*100
# Code for #6
Sage.Env.USGS$L.D.RelCov <- ((Total$ARTR2 + Total$ARTR2.D)/rowSums(Total))*100
# Combine for #5
Sage.Env.USGS$L.D.PropCover <- Sage.Env.USGS$L.RelCov/Sage.Env.USGS$L.D.RelCov

# 5
# Sage relative cover (live+dead)
# add all points of a spp. in a plot, then sum ARTR2 and ARTR2.D. Next divide by the total number of individuals of all species.

# Completed in #4
#Sage.Env.USGS$L.D.RelCov <- ((Total$ARTR2 + Total$ARTR2.D)/rowSums(Total))*100

# 6  
# Sage relative cover (live)
# add all points of a spp. in a plot, then take only ARTR2 and  divide by the total number of individuals of all species.

# Completed in #4
#Sage.Env.USGS$L.RelCov <- ((Total$ARTR2)/rowSums(Total))*100


# 7 
# Perrenial Grass relative cover

lpi <- read.csv("F:/LPI/USGSLPIRelativeCoverCommonInExcel.csv")
lpi <- lpi[-c(160:162),] # remove SUMS and COUNTIF rows at the bottom
row.names(lpi)<-lpi$X
lpi <- lpi[,-1] # remove extra plot id column
Sage.Env.USGS$PG.RelCov <- (lpi$ACHY+lpi$ARPU9+lpi$BOGR2+lpi$BOGR2.D+lpi$ELEL+lpi$HECO26+lpi$HECO26.D+lpi$SPCR+lpi$SPCR.D+lpi$HIJA)

#######################################################################
# Remove USGS, only April Data
Sage.Env.April <- Sage.Env.USGS[-c(1:60),] # remove usgs data
# Write USGS and April Data
write.csv(Sage.Env.April,file="F:/SageNMDSvariables/Sage.Env.April.csv")
write.csv(Sage.Env.USGS,file="F:/SageNMDSvariables/Sage.Env.USGS.csv")

