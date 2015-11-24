library(VGAM)

u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)

u.count <- u.den[c(1:37),]*90
a.count <- u.den[c(38:136),]*150
count <- rbind(u.count,a.count)

u$ARTR2 <- count$ARTR2
u1 <- subset(u, select = -c(Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,SlopeShape,DepthClass,H1.Texture,H1.SandSize))
u2 <- subset(u, select = -c(DWAClay,DWASand,DWApH,DWA.AWHC,AWHC.50,Sand.50,H1.Texture,H1.SandSize,Tot.Texture,Tot.SandSize,PedonDepth,Depth200,SlopeShape))



model.pois = glm(ARTR2 ~ ., data = u1, family = poisson,offset=u.den$ARTR2)
summary(model.pois)


pchisq(summary(model.pois)$deviance,
           summary(model.pois)$df.residual
)



# Zero Inflated
library(pscl)
model.zip = zeroinfl(ARTR2 ~ ., data = u1,offset=u.den$ARTR2,dist="poisson")
summary(model.zip)


cbind(nd, 
      Count = predict(model.zip, newdata = nd, type = "count"),
      Zero = predict(model.zip, newdata = nd, type = "zero")
)