# Health

library(plyr)

### % Dry Weight ### (PDW)
# Dry/Fresh
# if plant is better hydrated the # will be low
# if plant is dessicated # will be higher


### Specific Leaf Area ### (SLA) 
# m2/g(dry) 
# thicker leaves less efficient but tougher (lower SLA)
# look at other peoples SLA
# typical (Bruce Bugbee) is 100 to 600

LA<-read.csv("F:/Health/LeafAreaEpidermalConductance.csv",header=TRUE)
LA <- LA[ which(!LA$Wet.Dry<0.000000), ] # Remove negative weights.
LA <- LA[,c(1,14:15)]

NP<-read.csv("F:/Health/April Sagebrush N and Protein.csv",header=TRUE)
NP <- NP[,c(2,5:6)]

# find means from LA
pdw <- ddply( LA, 'Plot', summarize, pdw = mean(PctDryWeight, na.rm = T))
sla <- ddply( LA, 'Plot', summarize, sla = mean(SLAcm, na.rm = T))
LA <- join(pdw, sla, by = 'Plot', type = 'inner')

# What plots do LA and NP have in common?
LA.NP <- merge(NP, LA, by=c("Plot")) 




Height <- read.csv("F:/ShrubDensity/HeightClass/Output/AprilLiveDeadPlotbySizeClass.csv",header=TRUE)
Height <- rename(Height, c("X"="Plot"))
HeightM2 <- read.csv("F:/ShrubDensity/HeightClass/Output/AprilSizeClassLiveDeadDensityM2.csv",header=TRUE,row.names=1)


Hla <- merge(LA, Height, by=c("Plot"))
Hnp <- merge(NP, Height, by=c("Plot"))

plot(Hla$pdw, Hla$sla, main="Scatterplot Example", 
     xlab="pdw ", ylab="sla", pch=19)

plot(Hnp$NitrogePct, Hnp$ProteinPct, main="Scatterplot Example", 
     xlab="NitrogenPct ", ylab="ProteinPct", pch=19)




# Simple Bar Plot 
barplot(HeightM2$A, main="young Sagebrush", 
        xlab="Number of Sagebrush")


barplot(HeightM2$E, main=">100 Sagebrush", 
        xlab="Number of Sagebrush")

A <- count(Height$A)
B <- count(Height$B)
C <- count(Height$C)
D <- count(Height$D)
E <- count(Height$E)


E <- as.matrix(E)
E <- as.vector(E)

barplot(E)



