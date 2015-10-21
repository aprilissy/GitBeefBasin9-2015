#April's Shrub Data
#install.packages('plyr')
library(plyr)

# read in shrub density data
# usgs is april and usgs data, april is only april data
usgs <- read.csv('F:/ShrubDensity/PresenceAbsence/PlantDenSpeciesSummary 10-21.csv')
april <- usgs[3678:11140,]

#Put into Plot by Species matrix, sums shrub totals across transects 2, 3, and 4.
USGSplotXspp<-xtabs(Total~Plot+Species, usgs)
write.csv(USGSplotXspp,file="F:/ShrubDensity/PresenceAbsence/Output/USGSplotXspp.csv")

################################################################
###  April Data

AprilplotXspp <- USGSplotXspp[61:159,]
write.csv(AprilplotXspp,file="F:/ShrubDensity/PresenceAbsence/Output/AprilplotXspp.csv")

################################################################
###  N&S Plain Data

NSPlainplotXspp <- USGSplotXspp[c("24","38","40","42","43","80","82"),]
NSPlainplotXspp <- rbind(AprilplotXspp,NSPlainplotXspp)
write.csv(NSPlainplotXspp,file="F:/ShrubDensity/PresenceAbsence/Output/NSPlainplotXspp.csv")

########## Shrub Density in m2 and ha ##################

#know area sampled: 30m transects X 2m belt X 3 transects per plot (2,3,&4) = 180m2
# density in m2
USGSdensityM2 <- USGSplotXspp/180
write.csv(USGSdensityM2,file="F:/ShrubDensity/PresenceAbsence/Output/USGSShrubDensityM2.csv")

AprildensityM2 <- AprilplotXspp/180
write.csv(AprildensityM2,file="F:/ShrubDensity/PresenceAbsence/Output/AprilShrubDensityM2.csv")

NSPlaindensityM2 <- NSPlainplotXspp/180
write.csv(NSPlaindensityM2,file="F:/ShrubDensity/PresenceAbsence/Output/NSPlainShrubDensityM2.csv")

# #density in ha
# # ha = m2/10,000
# densityHa <- densityM2/10000
# densityHa
# write.csv(densityHa,file="F:/ShrubDensity/PresenceAbsence/Output/AprilShrubDensityHa.csv")

########### Presence/Absence ######################

#Turn shurb totals into presence/absence (1/0)
Upa <- (USGSplotXspp>0)     # logical, or 
Upa <- (USGSplotXspp>0)*1L  # integer 01 
write.csv(Upa,file="F:/ShrubDensity/PresenceAbsence/Output/USGSShrubPresenceAbsence.csv")

Apa <- (AprilplotXspp>0)     # logical, or 
Apa <- (AprilplotXspp>0)*1L  # integer 01 
write.csv(Apa,file="F:/ShrubDensity/PresenceAbsence/Output/AprilShrubPresenceAbsence.csv")

NSpa <- (NSPlainplotXspp>0)     # logical, or 
NSpa <- (NSPlainplotXspp>0)*1L  # integer 01 
write.csv(NSpa,file="F:/ShrubDensity/PresenceAbsence/Output/NSPlainShrubPresenceAbsence.csv")


#Select only ARTR and ARTR/D presence/absence (1/0)
USage <- Upa[,8:9]
write.csv(USage,file="F:/ShrubDensity/PresenceAbsence/Output/USGSSagePresenceAbsence.csv")

ASage <- Apa[,8:9]
write.csv(ASage,file="F:/ShrubDensity/PresenceAbsence/Output/AprilSagePresenceAbsence.csv")

NSSage <- NSpa[,8:9]
write.csv(NSSage,file="F:/ShrubDensity/PresenceAbsence/Output/NSPlainSagePresenceAbsence.csv")