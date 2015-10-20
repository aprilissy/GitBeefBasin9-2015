library(plyr)
library(dplyr)

# LPI file too big for DIMA, broke it up into soil types to download
# Use the line totals tab from .xlsx file 
begay<-read.csv('F:/LPI/Reports/LPIindicatorsBEGAY.csv')
ignacio<-read.csv('F:/LPI/Reports/LPIindicatorsIGNACIOLEANTO.csv')
mido<-read.csv('F:/LPI/Reports/LPIindicatorsMIDO.csv')

#Combined back into one LPI file
lpi3<-rbind(begay, ignacio, mido)
write.csv(lpi3,file="F:/LPI/Output/AprilLPIofAll3Soils.csv")

lpi3$Any.Hit.N[is.na(lpi3$Any.Hit.N)] <- lpi3$X1st.Hit.N[is.na(lpi3$Any.Hit.N)]; lpi3

#Put into plot by species matrix
lpiApril<-xtabs(Any.Hit.N~Plot+Indicator, lpi3)
write.csv(lpiApril,file="F:/LPI/Output/AprilLPIplotXspp.csv",row.names=TRUE)
lpiApril<-read.csv('F:/LPI/Output/AprilLPIplotXspp.csv',row.names=1)


# Combine DP and live of spp,
a <-select(lpiApril,contains(".DP"))
ls(a)
lpiApril$ARTR2=lpiApril$ARTR2+lpiApril$ARTR2.DP
lpiApril$ATCA2=lpiApril$ATCA2+lpiApril$ATCA2.DP
lpiApril$CHVI8=lpiApril$CHVI8+lpiApril$CHVI8.DP
lpiApril$HECO26=lpiApril$HECO26+lpiApril$HECO26.DP
lpiApril$KRLA2=lpiApril$KRLA2+lpiApril$KRLA2.DP
lpiApril$MAFR3=lpiApril$MAFR3+lpiApril$MAFR3.DP
lpiApril$OPPO=lpiApril$OPPO+lpiApril$OPPO.DP
lpiApril$SHRO=lpiApril$SHRO+lpiApril$SHRO.DP
lpiApril <- subset(lpiApril, select=-c(ARTR2.DP,ATCA2.DP,CHVI8.DP,HECO26.DP,KRLA2.DP,MAFR3.DP,OPPO.DP,SHRO.DP))
write.csv(lpiApril,file="F:/LPI/Output/AprilLPIplotXspp.csv",row.names=TRUE)

# Calculate % cover: (spp#hits/total possible hits)*100
pctcover <- (lpiApril/300)*100
pctcover[100,] <- colSums(pctcover != 0)
pctcover[101,] <- colSums(pctcover[c(1:99),])
sum <- (pctcover[c(100:101),])
pctcover <- (pctcover[-c(100:101),])

# Find the Common and Uncommon spp: Common occur in at least 5% of plots or have at least 5% coverage
Common <- pctcover[,(sum[1,] >= 5) | (sum[2,] > 5) ]
Uncommon <- pctcover[,(sum[1,] < 5) & (sum[2,] < 5) ]

# Remove uncommon spp. in by comparing to Common
lpiApril <- cbind(lpiApril[, which(colnames(lpiApril)%in% colnames(Common))])          

# Calculate density per m2
den<-lpiApril
den <- den/150 # (5 plots)*(30 meters)=150m
den <- subset(den, select=-c(Bare.Soil,Total.Foliar))
write.csv(den,file="F:/LPI/Output/AprilLPIDensityM2.csv", row.names=TRUE)

# Calculate Relative Cover:
# add all hits on a plot basis, then divide each cell by the sum of a row
# to get how many times a spp was hit relative to how many in plot.
lpiApril <- subset(lpiApril, select=-c(Bare.Soil,Total.Foliar))
relcoverApril<-lpiApril/rowSums(lpiApril)
write.csv(relcoverApril,file="F:/LPI/Output/AprilLPIRelativeCover.csv")
write.csv(lpiApril,file="F:/LPI/Output/AprilLPICommon.csv", row.names=TRUE)

#################################################################
### Add in USGS data
usgs<-read.csv('F:/LPI/Reports/LPIindicatorsUSGS.csv')
# Have to convert usgs plot names into factor not numeric
 # otherwise they get replaced with NA
usgs$Plot <- factor(usgs$Plot)
is.factor(usgs$Plot)

#Put into plot by species matrix
lpi<-xtabs(Any.Hit.N~Plot+Indicator, usgs)
write.csv(lpi,file="F:/LPI/Output/USGSLPIplotXspp.csv")
lpi<-read.csv('F:/LPI/Output/USGSLPIplotXspp.csv',row.names=1)

# Calculate density per m2
denu<-read.csv('F:/LPI/Output/USGSLPIplotXspp.csv',row.names=1)
denu <- denu/90 # (3 plots)*(30 meters)=90


#add all hits on a plot basis, then divide each cell by the sum of a row
# to get how many times a spp was hit relative to how many in plot.
relcover<-lpi/rowSums(lpi)
relcover
write.csv(relcover,file="F:/LPI/Output/USGSLPIRelativeCover.csv")

# read in RelativeCoverCommonInExcel
relu<-read.csv("F:/LPI/Output/USGSLPIRelativeCoverCommonInExcel.csv",header=TRUE,row.names=1)

# In denu, combine DP and live of spp,
library(dplyr)
b <-select(denu,contains(".DP"))
ls(b)
denu$AMUT=denu$AMUT+denu$AMUT.DP
denu$ARTR2=denu$ARTR2+denu$ARTR2.DP
denu$ATCA2=denu$ATCA2+denu$ATCA2.DP

denu$BOGR2=denu$BOGR2+denu$BOGR
denu$BOGR2.D=denu$BOGR2.D+denu$BOGR.D

denu$CHVI8=denu$CHVI8+denu$CHVI8.DP
denu$CHVI8.D=denu$CHVI8.D+denu$CHVI.D
denu$CHVI8=denu$CHVI8+denu$CHVI.DP

denu$GUSA=denu$GUSA+denu$GUSA.DP
denu$GUSA=denu$GUSA+denu$GUSA.DP

denu$HECO26=denu$HECO26+denu$HECO26.DP
denu$JUOS=denu$JUOS+denu$JUOS.DP
denu$KRLA2=denu$KRLA2+denu$KRLA2.DP
denu$MAFR3=denu$MAFR3+denu$MAFR3.DP
denu$OPPO=denu$OPPO+denu$OPPO.DP
denu$PIED=denu$PIED+denu$PIED.DP
denu$SHRO=denu$SHRO+denu$SHRO.DP
denu$YUBA=denu$YUBA+denu$YUBA.DP
denu <- subset(denu, select=-c(AMUT.DP,ARTR2.DP,ATCA2.DP,CHVI8.DP,CHVI,CHVI.D,CHVI.DP,GUSA.DP,HECO26.DP,JUOS.DP,KRLA2.DP,MAFR3.DP,OPPO.DP,PIED.DP,SHRO.DP,YUBA.DP))

# Then remove uncommon spp. in den by comparing to relu
denu <- cbind(denu[, which(colnames(denu)%in% colnames(relu))])          
write.csv(denu,file="F:/LPI/Output/USGSLPIDenM2.csv", row.names=TRUE)


#################################################################

# # Went though and changed lpiAll species names to growth type
# # (Grass, Shrub, Forb, Tree)
# # Gives us % of each growth form in each plot
# type <- read.csv ("F:/LPI/LPIpctType.csv")
# 
# lpitype<-xtabs(Any.Hit.Avg~Plot+Indicator, type)
# lpitype
# write.csv(lpitype, file="F:/LPI/lpitype.csv")
# 
# relcovertype<-lpitype/rowSums(lpitype)
# relcovertype
# write.csv(relcovertype,file="F:/LPI/LPIRelativeCoverType.csv")

