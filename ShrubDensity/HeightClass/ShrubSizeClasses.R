#April's Shrub Size Class Data
#install.packages('plyr')
library(plyr)

#read in shrub density detail data
class <- read.csv('F:/ShrubDensity/HeightClass/PlantDenDetail 8-21.csv')

#################################################################
## April's And USGS Data

class$total <- rowSums(class[,4:8]) #Sum all size classes for total density
total <- class[, c(1,2,3,9)] # Just total, size class removed
class <- class[,c (1:8)] # Just size class, total removed, for use in A-E below


#Plot by Size Class based on Speceis Code
class.order <- class[order(class$SpeciesCode),]  #sort a dataframe by the order of the elements in SpeciesCode
Sage.l <- class.order[c(573:1071),]
Sage.d <- class.order[c(1072:1543),]

write.csv(Sage.l,file="F:/ShrubDensity/HeightClass/USGSLivePlotXSizeClass.csv", row.names=FALSE)
write.csv(Sage.d,file="F:/ShrubDensity/HeightClass/USGSDeadPlotXSizeClass.csv", row.names=FALSE)

##################### 9/2/2015 #########################################
# I want plot by size class for live and for dead
# This is for use as environmental factor in NMDS

sage.l <- ddply(Sage.l, "Plot", numcolwise(sum)) # Sum all columns based on plot
sage.l <- sage.l[,-2] # remove transect total column
sage.l <- sage.l[-c(1:59),] # remove usgs data
write.csv(sage.l,file="F:/ShrubDensity/HeightClass/LivePlotbySizeClass.csv", row.names=FALSE)

sage.d <- ddply(Sage.d, "Plot", numcolwise(sum)) # Sum all columns based on plot
sage.d <- sage.d[,-2]# remove transect total column
sage.d <- sage.d[-c(1:59),] # remove usgs data
write.csv(sage.d,file="F:/ShrubDensity/HeightClass/DeadPlotbySizeClass.csv", row.names=FALSE)


############## Binary Size Classes #############################
##  this just means using relative cover (points hit/total points)
##  use this for NMDS instead
l <- read.csv('F:/ShrubDensity/HeightClass/LivePlotbySizeClass.csv',row.names=1)
d <- read.csv('F:/ShrubDensity/HeightClass/DeadPlotbySizeClass.csv',row.names=1)

total.l.d <- rowSums(l)+ rowSums(d)
l.rel.sage <- ((l)/(rowSums(l)+ rowSums(d)))*100
d.rel.sage <- ((d)/(rowSums(l)+ rowSums(d)))*100
write.csv(l.rel.sage,file="F:/ShrubDensity/HeightClass/LiveSizeClassSagePctCover.csv")
write.csv(d.rel.sage,file="F:/ShrubDensity/HeightClass/DeadSizeClassSagePctCover.csv")

TotalplotXspp<-xtabs(total~Plot+SpeciesCode, total) # put in plot by spp matrix
write.csv(TotalplotXspp,file="F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv")
Total <- read.csv("F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv",row.names=1)
Total <- Total[-c(1:60),] # remove usgs data
l.rel.total <- ((l)/rowSums(Total))*100
d.rel.total <- ((d)/rowSums(Total))*100
write.csv(l.rel.total,file="F:/ShrubDensity/HeightClass/LiveSizeClassTotalPctCover.csv")
write.csv(d.rel.total,file="F:/ShrubDensity/HeightClass/DeadSizeClassTotalPctCover.csv")

###### This sums shrub totals across transects 2,3, and 4 #####
###### And puts into Plot by Species matrix ###################

# Total Density
TotalplotXspp<-xtabs(total~Plot+SpeciesCode, total) # put in plot by spp matrix
write.csv(TotalplotXspp,file="F:/ShrubDensity/HeightClass/USGSTotalplotXspp.csv")

#Class A
AplotXspp<-xtabs(ClassAtotal~Plot+SpeciesCode, class)
write.csv(AplotXspp,file="F:/ShrubDensity/HeightClass/USGSAplotXspp.csv")

#Class B
BplotXspp<-xtabs(ClassBtotal~Plot+SpeciesCode, class)
write.csv(BplotXspp,file="F:/ShrubDensity/HeightClass/USGSBplotXspp.csv")

#Class C
CplotXspp<-xtabs(ClassCtotal~Plot+SpeciesCode, class)
write.csv(CplotXspp,file="F:/ShrubDensity/HeightClass/USGSCplotXspp.csv")

#Class D
DplotXspp<-xtabs(ClassDtotal~Plot+SpeciesCode, class)
write.csv(DplotXspp,file="F:/ShrubDensity/HeightClass/USGSDplotXspp.csv")

#Class E
EplotXspp<-xtabs(ClassEtotal~Plot+SpeciesCode, class)
write.csv(EplotXspp,file="F:/ShrubDensity/HeightClass/USGSEplotXspp.csv")


########## Shrub Density in m2 and ha ##################

#know area sampled: 30m transects X 2m belt X 3 transects per plot (2,3,& 4) = 180m2
# density in m2

# Total Density
TotaldensityM2 <- TotalplotXspp/180 # density per M2
write.csv(TotaldensityM2,file="F:/ShrubDensity/HeightClass/USGSTotalDensityM2.csv")

#Class A
AdensityM2 <- AplotXspp/180
write.csv(AdensityM2,file="F:/ShrubDensity/HeightClass/USGSAdensityM2.csv")

#Class B
BdensityM2 <- BplotXspp/180
write.csv(BdensityM2,file="F:/ShrubDensity/HeightClass/USGSBdensityM2.csv")

#Class C
CdensityM2 <- CplotXspp/180
write.csv(CdensityM2,file="F:/ShrubDensity/HeightClass/USGSCdensityM2.csv")

#Class D
DdensityM2 <- DplotXspp/180
write.csv(DdensityM2,file="F:/ShrubDensity/HeightClass/USGSDdensityM2.csv")

#Class E
EdensityM2 <- EplotXspp/180
write.csv(EdensityM2,file="F:/ShrubDensity/HeightClass/USGSEdensityM2.csv")


#density in ha
# ha = m2/10,000

# Total Density
TotaldensityHa <- TotaldensityM2/10000 # density per Ha
write.csv(TotaldensityHa,file="F:/ShrubDensity/HeightClass/USGSTotalDensityHa.csv")

#Class A
AdensityHa <- AdensityM2/10000
write.csv(AdensityHa,file="F:/ShrubDensity/HeightClass/USGSAdensityHa.csv")

#Class B
BdensityHa <- BdensityM2/10000
write.csv(BdensityHa,file="F:/ShrubDensity/HeightClass/USGSBdensityHa.csv")

#Class C
CdensityHa <- CdensityM2/10000
write.csv(CdensityHa,file="F:/ShrubDensity/HeightClass/USGSCdensityHa.csv")

#Class D
DdensityHa <- DdensityM2/10000
write.csv(DdensityHa,file="F:/ShrubDensity/HeightClass/USGSDdensityHa.csv")

#Class E
EdensityHa <- EdensityM2/10000
write.csv(EdensityHa,file="F:/ShrubDensity/HeightClass/USGSEdensityHa.csv")

#################################################################
## April's Data

# Remove USGS Data
Total <- TotalplotXspp[c(61:159),]
write.csv(Total,file="F:/ShrubDensity/HeightClass/AprilTotalplotXspp.csv")

A <- AplotXspp[c(61:159),]
write.csv(A,file="F:/ShrubDensity/HeightClass/AprilAplotXspp.csv")

B <- BplotXspp[c(61:159),]
write.csv(B,file="F:/ShrubDensity/HeightClass/AprilBplotXspp.csv")

C <- CplotXspp[c(61:159),]
write.csv(C,file="F:/ShrubDensity/HeightClass/AprilCplotXspp.csv")

D <- DplotXspp[c(61:159),]
write.csv(D,file="F:/ShrubDensity/HeightClass/AprilDplotXspp.csv")

E <- EplotXspp[c(61:159),]
write.csv(E,file="F:/ShrubDensity/HeightClass/AprilEplotXspp.csv")


########## Shrub Density in m2 and ha ##################

#know area sampled: 30m transects X 2m belt X 3 transects per plot (2,3,& 4) = 180m2
# density in m2

# Total Density
TotaldensityM2 <- Total/180 # density per M2
write.csv(TotaldensityM2,file="F:/ShrubDensity/HeightClass/AprilTotalDensityM2.csv")

#Class A
AdensityM2 <- A/180
write.csv(AdensityM2,file="F:/ShrubDensity/HeightClass/AprilAdensityM2.csv")

#Class B
BdensityM2 <- B/180
write.csv(BdensityM2,file="F:/ShrubDensity/HeightClass/AprilBdensityM2.csv")

#Class C
CdensityM2 <- C/180
write.csv(CdensityM2,file="F:/ShrubDensity/HeightClass/AprilCdensityM2.csv")

#Class D
DdensityM2 <- D/180
write.csv(DdensityM2,file="F:/ShrubDensity/HeightClass/AprilDdensityM2.csv")

#Class E
EdensityM2 <- E/180
write.csv(EdensityM2,file="F:/ShrubDensity/HeightClass/AprilEdensityM2.csv")


#density in ha
# ha = m2/10,000

# Total Density
TotaldensityHa <- Total/10000 # density per Ha
write.csv(TotaldensityHa,file="F:/ShrubDensity/HeightClass/AprilTotalDensityHa.csv")

#Class A
AdensityHa <- AdensityM2/10000
write.csv(AdensityHa,file="F:/ShrubDensity/HeightClass/AprilAdensityHa.csv")

#Class B
BdensityHa <- BdensityM2/10000
write.csv(BdensityHa,file="F:/ShrubDensity/HeightClass/AprilBdensityHa.csv")

#Class C
CdensityHa <- CdensityM2/10000
write.csv(CdensityHa,file="F:/ShrubDensity/HeightClass/AprilCdensityHa.csv")

#Class D
DdensityHa <- DdensityM2/10000
write.csv(DdensityHa,file="F:/ShrubDensity/HeightClass/AprilDdensityHa.csv")

#Class E
EdensityHa <- EdensityM2/10000
write.csv(EdensityHa,file="F:/ShrubDensity/HeightClass/AprilEdensityHa.csv")
