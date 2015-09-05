library(plyr)
# LPI file too big for DIMA, broke it up into soil types to download
# 'T' in LPIindicatorsTBegay.csv, etc... stands for transect because it has transects 1-5
begay<-read.csv('F:/LPI/LPIindicatorsTBEGAY.csv')
ignacio<-read.csv('F:/LPI/LPIindicatorsTIGNACIOLEANTO.csv')
mido<-read.csv('F:/LPI/LPIindicatorsTMIDO.csv')

#Combined back into one LPI file
lpi3<-rbind(begay, ignacio, mido)
lpi3
write.csv(lpi3,file="F:/LPI/AprilLPIofAll3Soils.csv")

#Put into plot by species matrix
lpiApril<-xtabs(Any.Hit.N~Plot+Indicator, lpi3)
lpiApril
write.csv(lpiApril,file="F:/LPI/AprilLPIplotXspp.csv")

#add all hits on a plot basis, then divide each cell by the sum of a row
# to get how many times a spp was hit relative to how many in plot.
relcoverApril<-lpiApril/rowSums(lpiApril)
relcoverApril
write.csv(relcoverApril,file="F:/LPI/AprilLPIRelativeCover.csv")


#### In Excel I used SUM to find the colum sums giving me the 
#### total number of times each species was hit across all plots.
#### Then I used COUNTIF giving me the total number of 
#### sites each species was found at.
#### Then I manually removed those with COUNTIF below 5 
#### AND SUMS below 0.05 (had to both be below 5 to be removed)
#### (Occur in at least 5% of plots or have at least 5% coverage)


#################################################################
### Add in USGS data
usgs<-read.csv('F:/LPI/LPIindicatorsTUSGS.csv')
# Have to convert usgs plot names into factor not numeric
 # otherwise they get replaced with NA
usgs$Plot <- factor(usgs$Plot)
is.factor(usgs$Plot)

#Combined into one LPI file
All<-rbind(lpi3, usgs)
All
write.csv(All,file="F:/LPI/USGSLPIofAprilAndUSGS.csv")

#Put into plot by species matrix
lpi<-xtabs(Any.Hit.N~Plot+Indicator, All)
lpi
write.csv(lpi,file="F:/LPI/USGSLPIplotXspp.csv")

#add all hits on a plot basis, then divide each cell by the sum of a row
# to get how many times a spp was hit relative to how many in plot.
relcover<-lpi/rowSums(lpi)
relcover
write.csv(relcover,file="F:/LPI/USGSLPIRelativeCover.csv")

#### In Excel I used SUM to find the colum sums giving me the 
#### total number of times each species was hit across all plots.
#### Then I used COUNTIF giving me the total number of 
#### sites each species was found at.
#### Then I manually removed those with COUNTIF below 5 
#### AND SUMS below 0.05 (had to both be below 5 to be removed)
#### (Occur in at least 5% of plots or have at least 5% coverage)

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

