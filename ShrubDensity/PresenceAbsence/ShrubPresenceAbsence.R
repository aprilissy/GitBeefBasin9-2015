#April's Shrub Data
#install.packages('plyr')
library(plyr)

# read in shrub density data
# usgs is april and usgs data, april is only april data
usgs <- read.csv('F:/ShrubDensity/PresenceAbsence/PlantDenSpeciesSummary 8-21.csv')
april <- usgs[3678:11140,]

#Put into Plot by Species matrix, sums shrub totals across transects 2, 3, and 4.
plotXspp<-xtabs(Total~Plot+Species, usgs)
plotXspp
write.csv(plotXspp,file="F:/ShrubDensity/PresenceAbsence/USGSplotXspp.csv")

################################################################
###  April Data

AprilplotXspp <- plotXspp[61:159,]
write.csv(AprilplotXspp,file="F:/ShrubDensity/PresenceAbsence/AprilplotXspp.csv")

########## Shrub Density in m2 and ha ##################

#know area sampled: 30m transects X 2m belt X 3 transects per plot (2,3,&4) = 180m2
# density in m2
densityM2 <- AprilplotXspp/180
densityM2
write.csv(densityM2,file="F:/ShrubDensity/PresenceAbsence/AprildensityM2.csv")

#density in ha
# ha = m2/10,000
densityHa <- densityM2/10000
densityHa
write.csv(densityHa,file="F:/ShrubDensity/PresenceAbsence/AprildensityHa.csv")

########### Presence/Absence ######################

#Turn shurb totals into presence/absence (1/0)
pa <- (AprilplotXspp>0)     # logical, or 
pa <- (AprilplotXspp>0)*1L  # integer 01 
pa
write.csv(pa,file="F:/ShrubDensity/PresenceAbsence/AprilShrubPresenceAbsence.csv")

#Select only ARTR and ARTR/D presence/absence (1/0)
Sage <- pa[,8:9]
write.csv(Sage,file="F:/ShrubDensity/PresenceAbsence/AprilSagePresenceAbsence.csv")

#Select only ARTR presence/absence (1/0)
SageLive <- pa[,8]
write.csv(SageLive,file="F:/ShrubDensity/PresenceAbsence/AprilSageLivePresenceAbsence.csv")

#Select only ARTR/D presence/absence (1/0)
SageDead <-pa[,9]
write.csv(SageDead,file="F:/ShrubDensity/PresenceAbsence/AprilSageDeadPresenceAbsence.csv")


################################################################
###  USGS Data

########## Shrub Density in m2 and ha ##################

#know area sampled: 30m transects X 2m belt X 3 transects per plot (2,3,&4) = 180m2
# density in m2
densityM2 <- plotXspp/180
densityM2
write.csv(densityM2,file="F:/ShrubDensity/PresenceAbsence/USGSdensityM2.csv")

#density in ha
# ha = m2/10,000
densityHa <- densityM2/10000
densityHa
write.csv(densityHa,file="F:/ShrubDensity/PresenceAbsence/USGSdensityHa.csv")

########### Presence/Absence ######################
 ### NOTE!!! These are only PA for shrub density data
 ### Must add in PA data from LPI

#Turn shurb totals into presence/absence (1/0)
pa <- (plotXspp>0)     # logical, or 
pa <- (plotXspp>0)*1L  # integer 01 
pa
write.csv(pa,file="F:/ShrubDensity/PresenceAbsence/USGSShrubPresenceAbsence.csv")

#Select only ARTR and ARTR/D presence/absence (1/0)
Sage <- pa[,8:9]
write.csv(Sage,file="F:/ShrubDensity/PresenceAbsence/USGSSagePresenceAbsence.csv")

#Select only ARTR presence/absence (1/0)
SageLive <- pa[,8]
write.csv(SageLive,file="F:/ShrubDensity/PresenceAbsence/USGSSageLivePresenceAbsence.csv")

#Select only ARTR/D presence/absence (1/0)
SageDead <-pa[,9]
write.csv(SageDead,file="F:/ShrubDensity/PresenceAbsence/USGSSageDeadPresenceAbsence.csv")