########  Univariate PreScreening  #########
# April Darger 2/5/2015
# Using notes from Susan Durham, Statistician and 
## modifying code from 'Exploratory With Colby General Linear Model' R code

library(car) 
library(MASS)


# Import Data Sets
Soils <- read.csv("F:/Soils/SoilEnvironmentaldataApril.csv", header=T) # All soil environmental data
Density <- read.csv("F:/ShrubDensity/HeightClass/AprilTotalDensityM2.csv", header=T) # Total density per m2 for april's data

# Clean and Combine
rownames(Soils) <- NULL

Density <- Density[, colSums(Density != 0) > 0] # remove columns with only 0 values where there were no shrubs of that species found.
Density <- cbind(Density, Soils) # combine soil and sagebrush density data
Density <- Density[,-20]# remove column 22 which was a second column of unique id(plots)
rownames(Density) <- NULL
write.csv(Density, file="F:/UnivariatePrescreening/DensitySoils.csv")

sumSage <- cbind(Soils,SageClass) # combine soil and sagebrush presence class data.
sumSage <- sumSage[,-32] # remove column 32 which was a second column of unique id(plots)
rownames(sumSage) <- NULL
write.csv(sumSage,file="F:/UnivariatePrescreening/SageClassSoils.csv")



# Looking at Sagebrush Class and Continuous Soil Predictors
#
boxplot(Elevation~Class,data = sumSage, main= 'Elevation')
boxplot(minClay~Class,data = sumSage, main= 'minClay')
boxplot(maxClay~Class,data = sumSage, main= 'maxClay')
boxplot(minSand~Class,data = sumSage, main= 'minSand')
boxplot(maxSand~Class,data = sumSage, main= 'maxSand')
boxplot(maxDepth~Class,data = sumSage, main= 'maxDepth')
boxplot(maxpH~Class,data = sumSage, main= 'maxpH')
boxplot(minpH~Class,data = sumSage, main= 'minpH')
boxplot(MaxAWC~Class,data = sumSage, main= 'MaxAWC')
boxplot(TotalAWC~Class,data = sumSage, main= 'TotalAWC')
boxplot(AWC25~Class,data = sumSage, main= 'AWC25')
boxplot(AWC50~Class,data = sumSage, main= 'AWC50')
boxplot(AWC100~Class,data = sumSage, main= 'AWC100')


### ANOVA for Continuous Variables ###
# 1a.

# This was done in SAS. See F:\UnivariatePrescreening\Continuous\ANOVA.docx for summary
sumSage$Class <- as.factor(sumSage$Class)
is.factor(sumSage$Class)
is.numeric(sumSage$maxSand)

aov.ex1 = aov(maxSand~Class,data=sumSage) #do the analysis of variance
summary(aov.ex1) #show the summary table
print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
boxplot(maxSand~Class,data=sumSage, main= 'Maximum Sand Percentage') #graphical summary


### Chi-Square test of independence for Categorical variables ###
# 1b.


#SlopeShape
tbl = table(sumSage$SlopeShape, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#maxDryValue
tbl = table(sumSage$maxDryValue, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#minDryValue
tbl = table(sumSage$minDryValue, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#maxDryChroma
tbl = table(sumSage$maxDryChroma, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#minDryChroma
tbl = table(sumSage$minDryChroma, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#maxMoistValue
tbl = table(sumSage$maxMoistValue, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#minMoistValue
tbl = table(sumSage$minMoistValue, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#maxMoistChroma
tbl = table(sumSage$maxMoistChroma, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#minMoistChroma
tbl = table(sumSage$minMoistChroma, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#CarbonateStage
tbl = table(sumSage$CarbonateStage, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#BioticCrustClass
tbl = table(sumSage$BioticCrustClass, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#Surface
tbl = table(sumSage$Surface, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#Subsurface
tbl = table(sumSage$Subsurface, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#Depth50
tbl = table(sumSage$Depth50, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl)

#Depth100
tbl = table(sumSage$Depth100, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#Depth150
tbl = table(sumSage$Depth150, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 

#Depth200
tbl = table(sumSage$Depth200, sumSage$Class)# Creates Soil by Class table
tbl # the contingency table 
chisq.test(tbl) #runs chi-square test of independence on variables.
ctbl = cbind(tbl[,"1"], tbl[,"2"] + tbl[,"3"]) # Combine Sage Dead and No Sage
ctbl 
chisq.test(ctbl) 


### Univariate prescreening for continuous response Density ###
# 2.

par(mar=c(1,1,1,1))
attach(Density)

#ARTR
scatterplot(Elevation, ARTR2,
            xlab="Elevation(m)", ylab="Artemesia tridentata per m2",
            main="ARTR2 and Elevation",
            labels=row.names(Density))
scatterplot(SlopeShape, ARTR2,
            xlab="SlopeShape", ylab="Artemesia tridentata per m2",
            main="ARTR2 and SlopeShape",
            labels=row.names(Density))
scatterplot(maxClay, ARTR2,
            xlab="maxClay", ylab="Artemesia tridentata per m2",
            main="ARTR2 and maxClay",
            labels=row.names(Density))
scatterplot(minClay, ARTR2,
            xlab="minClay", ylab="Artemesia tridentata per m2",
            main="ARTR2 and minClay",
            labels=row.names(Density))
scatterplot(maxSand, ARTR2,
            xlab="maxSand", ylab="Artemesia tridentata per m2",
            main="ARTR2 and maxSand",
            labels=row.names(Density))
scatterplot(minSand, ARTR2,
            xlab="minSand", ylab="Artemesia tridentata per m2",
            main="ARTR2 and minSand",
            labels=row.names(Density))
scatterplot(maxDepth, ARTR2,
            xlab="maxDepth", ylab="Artemesia tridentata per m2",
            main="ARTR2 and maxDepth",
            labels=row.names(Density))
scatterplot(maxpH, ARTR2,
            xlab="maxpH", ylab="Artemesia tridentata per m2",
            main="ARTR2 and maxpH",
            labels=row.names(Density))
scatterplot(minpH, ARTR2,
            xlab="minpH", ylab="Artemesia tridentata per m2",
            main="ARTR2 and minpH",
            labels=row.names(Density))
scatterplot(MaxAWC, ARTR2,
            xlab="MaxAWC", ylab="Artemesia tridentata per m2",
            main="ARTR2 and MaxAWC",
            labels=row.names(Density))
scatterplot(TotalAWC, ARTR2,
            xlab="TotalAWC", ylab="Artemesia tridentata per m2",
            main="ARTR2 and TotalAWC",
            labels=row.names(Density))
scatterplot(AWC25, ARTR2,
            xlab="AWC25", ylab="Artemesia tridentata per m2",
            main="ARTR2 and AWC25",
            labels=row.names(Density))
scatterplot(AWC50, ARTR2,
            xlab="AWC50", ylab="Artemesia tridentata per m2",
            main="ARTR2 and AWC50",
            labels=row.names(Density))
scatterplot(AWC100, ARTR2,
            xlab="AWC100", ylab="Artemesia tridentata per m2",
            main="ARTR2 and AWC100",
            labels=row.names(Density))



plot(Elevation, ARTR2, main="ARTR2 and Elevation", 
     xlab="Elevation (m)", ylab="Artemesia tridentata per m2", pch=19)
plot(SlopeShape, ARTR2, main="ARTR2 and SlopeShape", 
     xlab="SlopeShape", ylab="Artemesia tridentata per m2", pch=19)
plot(maxClay, ARTR2, main="ARTR2 and maxClay", 
     xlab="maxClay", ylab="Artemesia tridentata per m2", pch=19)
plot(minClay, ARTR2, main="ARTR2 and minClay", 
     xlab="minClay", ylab="Artemesia tridentata per m2", pch=19)
plot(maxSand, ARTR2, main="ARTR2 and maxSand", 
     xlab="maxSand", ylab="Artemesia tridentata per m2", pch=19)
plot(minSand, ARTR2, main="ARTR2 and minSand", 
     xlab="minSand", ylab="Artemesia tridentata per m2", pch=19)
plot(maxDepth, ARTR2, main="ARTR2 and maxDepth", 
     xlab="maxDepth", ylab="Artemesia tridentata per m2", pch=19)
plot(maxpH, ARTR2, main="ARTR2 and maxpH", 
     xlab="maxpH", ylab="Artemesia tridentata per m2", pch=19)
plot(minpH, ARTR2, main="ARTR2 and minpH", 
     xlab="minpH", ylab="Artemesia tridentata per m2", pch=19)
plot(MaxAWC, ARTR2, main="ARTR2 and MaxAWC", 
     xlab="MaxAWC", ylab="Artemesia tridentata per m2", pch=19)
plot(TotalAWC, ARTR2, main="ARTR2 and TotalAWC", 
     xlab="TotalAWC", ylab="Artemesia tridentata per m2", pch=19)
plot(AWC25, ARTR2, main="ARTR2 and AWC25", 
     xlab="AWC25", ylab="Artemesia tridentata per m2", pch=19)
plot(AWC50, ARTR2, main="ARTR2 and AWC50", 
     xlab="AWC50", ylab="Artemesia tridentata per m2", pch=19)
plot(AWC100, ARTR2, main="ARTR2 and AWC100", 
     xlab="AWC100", ylab="Artemesia tridentata per m2", pch=19)




#ARTR.D
scatterplot(Elevation, ARTR2.D,
            xlab="Elevation(m)", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and Elevation",
            labels=row.names(Density))
scatterplot(SlopeShape, ARTR2.D,
            xlab="SlopeShape", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and SlopeShape",
            labels=row.names(Density))
scatterplot(maxClay, ARTR2.D,
            xlab="maxClay", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and maxClay",
            labels=row.names(Density))
scatterplot(minClay, ARTR2.D,
            xlab="minClay", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and minClay",
            labels=row.names(Density))
scatterplot(maxSand, ARTR2.D,
            xlab="maxSand", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and maxSand",
            labels=row.names(Density))
scatterplot(minSand, ARTR2.D,
            xlab="minSand", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and minSand",
            labels=row.names(Density))
scatterplot(maxDepth, ARTR2.D,
            xlab="maxDepth", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and maxDepth",
            labels=row.names(Density))
scatterplot(maxpH, ARTR2.D,
            xlab="maxpH", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and maxpH",
            labels=row.names(Density))
scatterplot(minpH, ARTR2.D,
            xlab="minpH", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and minpH",
            labels=row.names(Density))
scatterplot(MaxAWC, ARTR2.D,
            xlab="MaxAWC", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and MaxAWC",
            labels=row.names(Density))
scatterplot(TotalAWC, ARTR2.D,
            xlab="TotalAWC", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and TotalAWC",
            labels=row.names(Density))
scatterplot(AWC25, ARTR2.D,
            xlab="AWC25", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and AWC25",
            labels=row.names(Density))
scatterplot(AWC50, ARTR2.D,
            xlab="AWC50", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and AWC50",
            labels=row.names(Density))
scatterplot(AWC100, ARTR2.D,
            xlab="AWC100", ylab="Dead Artemesia tridentata per m2",
            main="Dead ARTR2 and AWC100",
            labels=row.names(Density))



plot(Elevation, ARTR2.D, main="Dead ARTR2 and Elevation", 
     xlab="Elevation (m)", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(SlopeShape, ARTR2.D, main="Dead ARTR2 and SlopeShape", 
     xlab="SlopeShape", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(maxClay, ARTR2.D, main="Dead ARTR2 and maxClay", 
     xlab="maxClay", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(minClay, ARTR2.D, main="Dead ARTR2 and minClay", 
     xlab="minClay", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(maxSand, ARTR2.D, main="Dead ARTR2 and maxSand", 
     xlab="maxSand", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(minSand, ARTR2.D, main="Dead ARTR2 and minSand", 
     xlab="minSand", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(maxDepth, ARTR2.D, main="Dead ARTR2 and maxDepth", 
     xlab="maxDepth", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(maxpH, ARTR2.D, main="Dead ARTR2 and maxpH", 
     xlab="maxpH", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(minpH, ARTR2.D, main="Dead ARTR2 and minpH", 
     xlab="minpH", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(MaxAWC, ARTR2.D, main="Dead ARTR2 and MaxAWC", 
     xlab="MaxAWC", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(TotalAWC, ARTR2.D, main="Dead ARTR2 and TotalAWC", 
     xlab="TotalAWC", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(AWC25, ARTR2.D, main="Dead ARTR2 and AWC25", 
     xlab="AWC25", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(AWC50, ARTR2.D, main="Dead ARTR2 and AWC50", 
     xlab="AWC50", ylab="Dead Artemesia tridentata per m2", pch=19)
plot(AWC100, ARTR2.D, main="Dead ARTR2 and AWC100", 
     xlab="AWC100", ylab="Dead Artemesia tridentata per m2", pch=19)

### Relationships among predictor variables ###
# 3.

## Multicollinearity - reg procedure
# See SAS code in word document under Univariate Prescreening/Multicollinearity

## Dimension reduction (PCA)
#do PCA
D <- read.csv("F:/LPI/AprilLPIRelativeCoverCommonInExcel.csv")
rownames(D) <- NULL
myPCA = princomp(D[,2:NCOL(D)])
biplot(myPCA)
summary(myPCA)
loadings(myPCA)
# plot again with distance from
# water labels
#siteD = read.csv("WA_env_variables.csv")
#biplot(myPCA,xlabs=siteD$distclass)