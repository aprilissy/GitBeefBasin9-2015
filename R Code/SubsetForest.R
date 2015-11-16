u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)
u$ARTR2 <- u.den$ARTR2
u1 <- subset(u, select = -c(Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,SlopeShape,DepthClass,H1.Texture,H1.SandSize))
u2 <- subset(u, select = -c(DWAClay,DWASand,DWApH,DWA.AWHC,AWHC.50,Sand.50,H1.Texture,H1.SandSize,Tot.Texture,Tot.SandSize,PedonDepth,Depth200,SlopeShape))

u1[is.na(u1)] <- 0 # replace NA with 0

Boruta.live <- Boruta(ARTR2~., data = u1, doTrace = 2, ntree = 1000)
Boruta.live
TentativeRoughFix(Boruta.live, averageOver = Inf)


live.rf = randomForest(as.numeric(ARTR2) ~ .
                       , data = u1,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit, mtry = 2, 
                       ntree = 1000)


varImpPlot(live.rf, main = 'Live Sagebrush')


x <- u1[,c(1:34)]
y <- u1[,35]
rf.cv <- rfcv(x, y, cv.fold=10)

with(rf.cv, plot(n.var, error.cv))

library(rfUtilities)
multi.collinear(x, p = 0.05)




###
# Partial Dependence Plots
###

partialPlot(live.rf,u1, H1.ClayPercent, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u1, PedonDepth, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u1, MaxAWHC, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u1, Slope, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u1, Tot.SandSize, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u1, H1.MoistRed, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u1, H1.DryRed, main = 'Live Sagebrush Partial Dependence on ...')
