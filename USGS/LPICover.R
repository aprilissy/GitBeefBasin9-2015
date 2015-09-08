# Colby's and USGS cLHS Points
# LPI relative cover
# Use 'Indicators Report' from Jornada's DIMA database
# Choose 'Species' from indicator list
# Run report, load 'Plot Totals' tab into R
# 10/3/2014

library(plyr)
LPI<-read.csv("F:/USGS/IndicatorsReport.csv")

lpi<-xtabs(Any.Hit.Avg~Plot+Indicator, LPI)
lpi

relcover<-lpi/rowSums(lpi)
relcover
write.csv(relcover,file="F:/USGS/LPIRelativeCover.csv")
