library(randomForest)

# Read in Soil and LPI Data
soil <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
lpi <- read.csv("F:/LPI/Output/USGSLPIplotXspp.csv",header=TRUE, row.names=1)
belt <- read.csv("F:/ShrubDensity/PresenceAbsence/Output/USGSplotXspp.csv",header=TRUE, row.names=1)


# Remove Predetermined Variables (Covariance and VIF)
soil <- subset(soil, select = -c(DepthClass,Aspect,Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,H1.Texture,SlopeShape,Tot.SandSize,H1.SandSize,H1.DryRed,H1.DryValue,H1.DryCClass,Tot.DryRed,Tot.DryValue,Tot.DryCClass,MaxSand,MaxpH,MaxDryValue,MaxAWHC))

# Combine Shrub Data and Soils Data
soil$ARTR2 <- lpi$ARTR2
soil$ATCA2 <- lpi$ATCA2

# Which rows have NA present somewhere
rownames(soil)[rowSums(is.na(soil)) > 0]

# Replace NA with 0 (necesary for Boruta but not Random Forest)
soil[is.na(soil)] <- 0 # replace NA with 0



# Run Random Forest
live.rf = randomForest(as.numeric(ATCA2) ~ .
                       , data = soil,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,mtry=2,
                       ntree = 500)

(rf <- randomForest(x=soil,y=lpi$ARTR2,proximity=TRUE,
                    importance=TRUE,keep.forest=TRUE,
                    na.action = na.omit,mtry=2,
                    ntree = 501))


plot(live.rf)


varImpPlot(live.rf, sort=TRUE, main = 'Live Sagebrush')

plot( predict(live.rf), y)
abline(c(0,1),col=2)


# Shows values of varImpPlot. Type = MSE or Node Purity, , 2)=#of decimal places
a<-round(importance(live.rf,type=1), 2)
a[order(a[,1],decreasing=TRUE),]


# Look at how ntree changes Error
plot(live.rf, type="l", main=deparse(substitute(live.rf)))
