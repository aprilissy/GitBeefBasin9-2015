library(randomForest)

# Read in Soil and Shrub Density Data
usgs<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
usgs.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)

# Change Shrub Density per M2 to Count Data
usgs.count <- usgs.den[c(1:37),]*90
april.count <- usgs.den[c(38:136),]*150
count <- rbind(usgs.count,april.count)

# Combine ARTR Shrub Data and Soils Data
usgs$ARTR2 <- count$ARTR2

# Remove Predetermined Variables (Covariance and VIF)
usgs1 <- subset(usgs, select = -c(DepthClass,Aspect,Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,H1.Texture,SlopeShape,Tot.SandSize,H1.SandSize,H1.DryRed,H1.DryValue,H1.DryCClass,Tot.DryRed,Tot.DryValue,Tot.DryCClass,MaxSand,MaxpH,MaxDryValue,MaxAWHC))

rownames(usgs1)[rowSums(is.na(usgs1)) > 0]
usgs1[is.na(usgs1)] <- 0 # replace NA with 0



# Run Random Forest
live.rf = randomForest(as.numeric(ARTR2) ~ .
                       , data = usgs1,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,mtry=2,
                       ntree = 501)



varImpPlot(live.rf, sort=TRUE, main = 'Live Sagebrush')

round(importance(live.rf,type=1), 2)

plot(live.rf, type="l", main=deparse(substitute(live.rf)))
