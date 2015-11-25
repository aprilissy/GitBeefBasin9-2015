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




# op <- par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(mar=c(2,2,2,2))
# 
# importanceOrder=order(-live.rf$importance)
# names=rownames(live.rf$importance)[importanceOrder][1:15]
# par(mfrow=c(5, 3), xpd=NA)
# for (name in names)
#     partialPlot(live.rf, u,x.var=ARTR2, eval(name), main=name, xlab=name,ylim=c(-.2,.9))
# 
# par(op)



# x <- u[,c(1:24)]
# y <- u[,25]
# rf.cv <- rfcv(x, y, cv.fold=10)
# 
# with(rf.cv, plot(n.var, error.cv))
# 
# library(rfUtilities)
# multi.collinear(x, p = 0.05)




###
# Partial Dependence Plots
###

partialPlot(live.rf,u, H1.ClayPercent, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, PedonDepth, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, H1.DWA_AWC, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, H1.Texture, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, H1.MoistRed, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, DepthClass, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, H1.SandSize, main = 'Live Sagebrush Partial Dependence on ...')

partialPlot(live.rf,u, SlopeShape, main = 'Live Sagebrush Partial Dependence on ...')

