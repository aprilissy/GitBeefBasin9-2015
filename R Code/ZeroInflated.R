library(VGAM)

u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)

u.count <- u.den[c(1:37),]*90
a.count <- u.den[c(38:136),]*150
count <- rbind(u.count,a.count)

u$ARTR2 <- count$ARTR2
u1 <- subset(u, select = -c(DepthClass,Aspect,Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,H1.Texture,SlopeShape,Tot.SandSize,H1.SandSize,H1.DryRed,H1.DryValue,H1.DryCClass,Tot.DryRed,Tot.DryValue,Tot.DryCClass,MaxSand,MaxpH,MaxDryValue,MaxAWHC))


fit <- lm(as.formula(paste(colnames(u1)[23], "~",
                           paste(colnames(u1)[c(1:22)], collapse = "+"),
                           sep = "")),data=u1)
vif(fit)

model.pois = glm(ARTR2 ~ ., data = u1, family = poisson,offset=u.den$ARTR2)
summary(model.pois)
vif(model.pois) # variance inflation factors 


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
