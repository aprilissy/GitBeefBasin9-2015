soilsSum<-read.csv("F:/Soils/SoilEnvironmentalData.csv", header=T)
soilsOrg<-read.csv("C:/Users/Grant/Documents/Beef Basin 2013/SoilDataInputR.csv", header=T)
Sage<-read.csv("F:/ShrubDensity/PresenceAbsence/AprilSagePresenceAbsence.csv", header=T)
Live<-read.csv("F:/ShrubDensity/PresenceAbsence/AprilSageLivePresenceAbsence.csv")
Dead<-read.csv("F:/ShrubDensity/PresenceAbsence/AprilSageDeadPresenceAbsence.csv")
Density<-read.csv("F:/ShrubDensity/PresenceAbsence/AprildensityM2.csv",header=T)


sumSage<- cbind(soilsSum,Sage)

sumDensity<- cbind(soilsSum,Density)

g<-glm(as.factor(ARTR2)~maxDepth+minpH+BioticCrustClass+as.factor(CarbonateStage),data=sumSage, family= 'binomial')
summary(g)

h<-glm(as.factor(ARTR2.D)~maxDepth+minpH+minClay+maxpH+as.factor(CarbonateStage),data=sumSage, family= 'binomial')
summary(h)


plot(g)

boxplot(maxDepth~ARTR2,data = sumSage, main= 'maxDepth')
boxplot(minSand~ARTR2,data = sumSage, main= 'minSand')
boxplot(maxSand~ARTR2,data = sumSage, main= 'maxSand')
boxplot(minClay~ARTR2,data = sumSage, main= 'minClay')
boxplot(maxClay~ARTR2,data = sumSage, main= 'maxClay')
boxplot(BioticCrustClass~ARTR2,data = sumSage, main= 'BioticCrustClass')
boxplot(CarbonateStage~ARTR2,data = sumSage, main= 'CarbonateStage')
boxplot(maxpH~ARTR2,data = sumSage, main= 'maxpH')
boxplot(minpH~ARTR2,data = sumSage, main= 'minpH')


with(sumDensity, plot(Elevation, ARTR2))
title(main="Elevation (ft) vs. ARTR2")

with(sumDensity, plot(ARTR2, maxClay))
title(main="MaxClay (ft) vs. ARTR2")

