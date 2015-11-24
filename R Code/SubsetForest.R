library(Boruta)
library(randomForest)

u<-read.csv("F:/Soils/SoilSubset.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)
u$ARTR2 <- u.den$ARTR2

rownames(u)[rowSums(is.na(u)) > 0]
u[is.na(u)] <- 0 # replace NA with 0


Boruta.live <- Boruta(ARTR2~., data = u, doTrace = 2, ntree = 1000)
Boruta.live
TentativeRoughFix(Boruta.live, averageOver = Inf)
Boruta.live$finalDecision

live.rf = randomForest(as.numeric(ARTR2) ~ .
                       , data = u,proximity=TRUE,
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
