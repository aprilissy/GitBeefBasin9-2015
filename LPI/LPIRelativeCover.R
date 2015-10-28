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

# Fill in Bare Soil NA form 1st Hit N column.
lpi3$Any.Hit.N[is.na(lpi3$Any.Hit.N)] <- lpi3$X1st.Hit.N[is.na(lpi3$Any.Hit.N)]; lpi3

#Put into plot by species matrix
lpiApril<-xtabs(Any.Hit.N~Plot+Indicator, lpi3)
write.csv(lpiApril,file="F:/LPI/Output/AprilLPIplotXspp.csv",row.names=TRUE)
lpiApril<-read.csv('F:/LPI/Output/AprilLPIplotXspp.csv',row.names=1)
lpiApril <- subset(lpiApril, select=-c(Total.Foliar,Total.Litter))
write.csv(lpiApril,file="F:/LPI/Output/AprilLPIplotXspp.csv",row.names=TRUE)


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
write.csv(Common,file="F:/LPI/Output/AprilLPIPercentCover.csv", row.names=TRUE)

# Remove uncommon spp. in by comparing to Common
lpiApril <- cbind(lpiApril[, which(colnames(lpiApril)%in% colnames(Common))])          
write.csv(lpiApril,file="F:/LPI/Output/AprilLPICommon.csv", row.names=TRUE)

# Calculate density per m2
den<-lpiApril
den <- den/150 # (5 plots)*(30 meters)=150m
write.csv(den,file="F:/LPI/Output/AprilLPIDensityM2.csv", row.names=TRUE)

# # Calculate Relative Cover:
# # add all hits on a plot basis, then divide each cell by the sum of a row
# # to get how many times a spp was hit relative to how many in plot.
# lpiApril <- subset(lpiApril, select=-c(Bare.Soil,Total.Foliar))
# relcoverApril<-lpiApril/rowSums(lpiApril)
# write.csv(relcoverApril,file="F:/LPI/Output/AprilLPIRelativeCover.csv")

#################################################################
### Add in USGS data
usgs<-read.csv('F:/LPI/Reports/LPIindicatorsUSGS.csv')
# Convert usgs plot names into factor not numeric
 # otherwise they get replaced with NA
usgs$Plot <- factor(usgs$Plot)
is.factor(usgs$Plot)

# Fill in Bare Soil NA from 1st Hit N column.
usgs$Any.Hit.N[is.na(usgs$Any.Hit.N)] <- usgs$X1st.Hit.N[is.na(usgs$Any.Hit.N)]; usgs

#Put into plot by species matrix
lpi<-xtabs(Any.Hit.N~Plot+Indicator, usgs)
write.csv(lpi,file="F:/LPI/Output/USGSLPIplotXspp.csv")
lpi<-read.csv('F:/LPI/Output/USGSLPIplotXspp.csv',row.names=1)
lpi <- subset(lpi, select=-c(Total.Foliar,Total.Litter))
write.csv(lpi,file="F:/LPI/Output/USGSLPIplotXspp.csv")

# Keep only usgs sites that have both soil and veg data.
lpi <- lpi[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),]


# Combine DP and live of spp,
b <-select(lpi,contains(".DP"))
ls(b)
lpi$AMUT=lpi$AMUT+lpi$AMUT.DP
lpi$ARTR2=lpi$ARTR2+lpi$ARTR2.DP
lpi$ATCA2=lpi$ATCA2+lpi$ATCA2.DP
lpi$CHVI8=lpi$CHVI8+lpi$CHVI8.DP
lpi$GUSA2=lpi$GUSA2+lpi$GUSA2.DP
lpi$JUOS=lpi$JUOS+lpi$JUOS.DP
lpi$KRLA2=lpi$KRLA2+lpi$KRLA2.DP
lpi$MAFR3=lpi$MAFR3+lpi$MAFR3.DP
lpi$OPPO=lpi$OPPO+lpi$OPPO.DP
lpi$PIED=lpi$PIED+lpi$PIED.DP
lpi$SHRO=lpi$SHRO+lpi$SHRO.DP
lpi$YUBA=lpi$YUBA+lpi$YUBA.DP
lpi <- subset(lpi, select=-c(AMUT.DP,ARTR2.DP,ATCA2.DP,CHVI8.DP,GUSA2.DP,JUOS.DP,KRLA2.DP,MAFR3.DP,OPPO.DP,PIED.DP,SHRO.DP,YUBA.DP))

# Combine April and USGS
RBIND <- function(datalist) {
  require(plyr)
  temp <- rbind.fill(datalist)
  rownames(temp) <- unlist(lapply(datalist, row.names))
  temp
}
combined <- RBIND(list(lpi, lpiApril))
combined[is.na(combined)] <- 0
write.csv(combined,file="F:/LPI/Output/USGSLPIplotXspp.csv")

# Calculate % cover: (spp#hits/total possible hits)*100
usgspctcover <- ((combined[c(1:37),])/180)*100
aprilpctcover <- ((combined[c(38:136),])/300)*100
usgspctcover <- rbind(usgspctcover,aprilpctcover)

# Sum and Countif
usgspctcover[137,] <- colSums(usgspctcover != 0)
usgspctcover[138,] <- colSums(usgspctcover[c(1:136),])
usgssum <- (usgspctcover[c(137:138),])
usgspctcover <- (usgspctcover[-c(137:138),])

# Find the Common and Uncommon spp: Common occur in at least 5% of plots or have at least 5% coverage
usgsCommon <- usgspctcover[,(usgssum[1,] >= 5) | (usgssum[2,] >= 5) ]
usgsUncommon <- usgspctcover[,(usgssum[1,] < 5) & (usgssum[2,] < 5) ]
write.csv(usgsCommon,file="F:/LPI/Output/USGSLPIPercentCover.csv", row.names=TRUE)

# Remove uncommon spp. in by comparing to Common
combined <- cbind(combined[, which(colnames(combined)%in% colnames(usgsCommon))])          
write.csv(combined,file="F:/LPI/Output/USGSLPICommon.csv")

# Calculate density per m2
denu <- (combined[c(1:60),])/90 # (3 plots)*(30 meters)=90
dena <- (combined[c(61:159),])/180 # (5 plots)*(30 meters)=180
denu <- rbind(denu,dena)
write.csv(denu,file="F:/LPI/Output/USGSLPIDensityM2.csv")


# #add all hits on a plot basis, then divide each cell by the sum of a row
# # to get how many times a spp was hit relative to how many in plot.
# combined <- subset(combined, select=-c(Bare.Soil,Total.Foliar))
# relcover<-combined/rowSums(combined)
# relcover
# write.csv(relcover,file="F:/LPI/Output/USGSLPIRelativeCover.csv")


#################################################################
### April + USGS N and S Plain

combined <- read.csv("F:/LPI/Output/USGSLPIplotXspp.csv",row.names=1)
USGSinNSplain <- combined[c("24","38","40","42","43","80","82"),]
April1 <- combined[c(38:136),]
USGSinNSplain <- rbind(April1,USGSinNSplain)
write.csv(USGSinNSplain,file="F:/LPI/Output/NSLPIplotXspp.csv")

# Calculate % cover: (spp#hits/total possible hits)*100
nsupctcover <- ((USGSinNSplain[c(100:106),])/180)*100
nsapctcover <- ((USGSinNSplain[c(1:99),])/300)*100
nsupctcover <- rbind(nsupctcover,nsapctcover)

# Sum and Countif
nsupctcover[107,] <- colSums(nsupctcover != 0)
nsupctcover[108,] <- colSums(nsupctcover[c(1:106),])
nssum <- (nsupctcover[c(107:108),])
nsupctcover <- (nsupctcover[-c(107:108),])

# Find the Common and Uncommon spp: Common occur in at least 5% of plots or have at least 5% coverage
nsCommon <- nsupctcover[,(nssum[1,] >= 5) | (nssum[2,] >= 5) ]
nsUncommon <- nsupctcover[,(nssum[1,] < 5) & (nssum[2,] < 5) ]
write.csv(nsCommon,file="F:/LPI/Output/NSLPIPercentCover.csv", row.names=TRUE)

# Remove uncommon spp. in by comparing to Common
USGSinNSplain <- cbind(USGSinNSplain[, which(colnames(USGSinNSplain)%in% colnames(nsCommon))])          
write.csv(USGSinNSplain,file="F:/LPI/Output/NSLPICommon.csv")


# Calculate density per m2
dennsu <- (combined[c(100:106),])/90 # (3 plots)*(30 meters)=90
dennsa <- (combined[c(1:99),])/180 # (5 plots)*(30 meters)=180
dennsu <- rbind(denu,dena)
write.csv(dennsu,file="F:/LPI/Output/NSLPIDensityM2.csv")

# #add all hits on a plot basis, then divide each cell by the sum of a row
# # to get how many times a spp was hit relative to how many in plot.
# combined <- subset(combined, select=-c(Bare.Soil,Total.Foliar))
# relcover<-combined/rowSums(combined)
# relcover
# write.csv(relcover,file="F:/LPI/Output/NSLPIRelativeCover.csv")


#################################################################
# Look only at ARTR,ATCA,KRLA,BOGR,SPCR,

Acommon <- read.csv('F:/LPI/Output/AprilLPICommon.csv',row.names=1)
Ucommon <- read.csv('F:/LPI/Output/USGSLPICommon.csv',row.names=1)
NScommon <- read.csv('F:/LPI/Output/NSLPICommon.csv',row.names=1)


# What are the two veg in USGS not in April? SPCO and HIJA
difference <- cbind(Ucommon[, -which(colnames(Acommon)%in% colnames(Ucommon))])          


Acommon <- subset(Acommon, select=c(ARTR2,ARTR2.D,ATCA2,ATCA2.D,KRLA2,BOGR2,BOGR2.D,SPCR,SPCR.D,HECO26,HECO26.D,Bare.Soil))
Ucommon <- subset(Ucommon, select=c(ARTR2,ARTR2.D,ATCA2,ATCA2.D,KRLA2,BOGR2,BOGR2.D,SPCR,SPCR.D,HECO26,HECO26.D,Bare.Soil))
NScommon <- subset(NScommon, select=c(ARTR2,ARTR2.D,ATCA2,ATCA2.D,KRLA2,BOGR2,BOGR2.D,SPCR,SPCR.D,HECO26,HECO26.D,Bare.Soil))

APG <- subset(Acommon, select=-c(BOGR2,BOGR2.D,SPCR,SPCR.D,HECO26,HECO26.D))
UPG <- subset(Ucommon, select=-c(BOGR2,BOGR2.D,SPCR,SPCR.D,HECO26,HECO26.D))
NSPG <- subset(NScommon, select=-c(BOGR2,BOGR2.D,SPCR,SPCR.D,HECO26,HECO26.D))

APG$PG <- Acommon$BOGR2+Acommon$BOGR2.D+Acommon$SPCR+Acommon$SPCR.D+Acommon$HECO26+Acommon$HECO26.D
UPG$PG <- Ucommon$BOGR2+Ucommon$BOGR2.D+Ucommon$SPCR+Ucommon$SPCR.D+Ucommon$HECO26+Ucommon$HECO26.D
NSPG$PG <- NScommon$BOGR2+NScommon$BOGR2.D+NScommon$SPCR+NScommon$SPCR.D+NScommon$HECO26+NScommon$HECO26.D
