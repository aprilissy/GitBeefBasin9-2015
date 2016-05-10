library(randomForest)

# Read in Soil and LPI Data
soil <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
lpi <- read.csv("F:/LPI/Output/USGSLPIplotXspp.csv",header=TRUE, row.names=1)
belt <- read.csv("F:/ShrubDensity/PresenceAbsence/Output/USGSplotXspp.csv",header=TRUE, row.names=1)

soil <- read.csv("E:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
lpi <- read.csv("E:/LPI/Output/USGSLPIplotXspp.csv",header=TRUE, row.names=1)
belt <- read.csv("E:/ShrubDensity/PresenceAbsence/Output/USGSplotXspp.csv",header=TRUE, row.names=1)

# Keep only usgs sites that have both soil and veg data.
beltApril <- belt[c(61:159),]
beltUSGS <- belt[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),]
belt <- rbind(beltApril, beltUSGS)

# Remove Predetermined Variables (Covariance and VIF)
soil <- subset(soil, select = -c(DepthClass,Aspect,Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,H1.Texture,SlopeShape,Tot.SandSize,H1.SandSize,H1.DryRed,H1.DryValue,H1.DryCClass,Tot.DryRed,Tot.DryValue,Tot.DryCClass,MaxSand,MaxpH,MaxDryValue,MaxAWHC))

# Combine Plant Data and Soils Data
soilplant <- soil
#soilplant$ARTR2 <- lpi$ARTR2
soilplant$ATCA2 <- lpi$ATCA2


# Which rows have NA present somewhere
rownames(soil)[rowSums(is.na(soil)) > 0]

# Replace NA with 0 (necesary for Boruta but not Random Forest)
soil[is.na(soil)] <- 0 # replace NA with 0

soilplant <- na.omit(soilplant)

# Run Random Forest
live.rf = randomForest(as.numeric(ATCA2) ~ .
                       , data = soilplant,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,mtry=2,
                       ntree = 500)

#var explained printed
print(live.rf)


(rf <- randomForest(x=soil,y=as.numeric(lpi$ATCA2),proximity=TRUE,
                    importance=TRUE,keep.forest=TRUE,
                    na.action = na.omit,mtry=2,
                    ntree = 501))


plot(live.rf)
plot(rf)

varImpPlot(live.rf, sort=TRUE, main = 'Live Sagebrush')
varImpPlot(rf, sort=TRUE, main = 'Live Sagebrush')


plot( predict(live.rf), y=live.rf$y)
abline(c(0,1),col=2)

plot( predict(rf), y=lpi$ARTR2)
abline(c(0,1),col=2)

partialPlot(live.rf, soilplant, PedonDepth)
partialPlot(live.rf, soilplant, DWApH)


# Shows values of varImpPlot. Type = MSE or Node Purity, , 2)=#of decimal places
a <- round(importance(live.rf,type=1), 2)

SoilVariables <- rownames(a)
rownames(a) <- NULL
a <- cbind(SoilVariables,a)
a <- a[order(a[,2],decreasing=TRUE),]



# Look at how ntree changes Error
plot(live.rf, type="l", main=deparse(substitute(live.rf)))


##small test
test <- soilplant[,c(23,10,12,20,21,22)]
test$MaxEffervescence <- as.ordered(test$MaxEffervescence)
test.rf = randomForest(as.numeric(ATCA2) ~ .
                       , data = test,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,
                       ntree = 500)
test.rf
plot(test.rf)

varImpPlot(test.rf, sort=TRUE, main = 'Live Sagebrush')

plot( predict(test.rf), y=test.rf$y)
abline(c(0,1),col=2)

partialPlot(test.rf, soilplant, H1.pH)
partialPlot(test.rf, soilplant, DWApH)
partialPlot(test.rf, soilplant, MaxEffervescence)

pairs(test)

library(quantregForest)
test$ATCA2 <- as.numeric(test$ATCA2)
qrf <- quantregForest(x=test[-1], y=test$ATCA2, importance=TRUE)
## plot out-of-bag predictions for the training data
plot(qrf)
## compute out-of-bag predictions
quant.outofbag <- predict(qrf)
## predict test data using all obervations per node for prediction
quant.newdata <- predict(qrf, newdata= Xtest, all=TRUE)
## look at computed importance measure of predictors
importance(qrf)
qrf
summary(qrf)
varImpPlot.qrf(qrf)
