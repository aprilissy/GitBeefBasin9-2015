# April Darger
# STAT 5600
# Final Project
library(rpart)
library(verification)
library(randomForest)
library(rpart.plot)
library(RColorBrewer)
library(Boruta)
library(plyr)
#library(rattle)

#The prp() has a lot of different options to check out
#prp(model)

#This one has the best default
#fancyRpartPlot(model)


# read in and configure april and usgs shrub count data
d.usgs <- read.csv("F:/ShrubDensity/PresenceAbsence/Output/USGSplotXspp.csv", row.names=1)
d.usgs[is.na(d.usgs)] <- 0 # replace NA with 0
d.april <- d.usgs[-c(1:60),] # pull out april data
d.usgs <- d.usgs[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),] # keep only veg with soils data
d.usgs <- rbind(d.usgs,d.april) # combine april and usgs (with only those that have soils data) into one
d.usgs <- d.usgs[ order(row.names(d.usgs)), ] # Order so row.names matches soils data

# Pull out ARTR2 and ARTR2.D
d.usgs.l <- d.usgs$ARTR2
d.usgs.d <- d.usgs$ARTR2.D
d.april.l <- d.april$ARTR2
d.april.d <- d.april$ARTR2.D
d.usgs.ld <- d.usgs$ARTR2 + d.usgs$ARTR2.D # add ARTR & ARTR.D into ARTR.LD
d.april.ld <- d.april$ARTR2 + d.april$ARTR2.D # add ARTR & ARTR.D into ARTR.LD

# Read in and configure april and usgs soils data
s.usgs <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",row.names=1)
s.usgs[is.na(s.usgs)] <- 0 # replace NA with 0
s.april <- s.usgs[c(1:99),] # pull our april data
s.usgs <- s.usgs[ order(row.names(s.usgs)), ] # Order so row.names matches veg data

# Combine ARTR2, ARTR2.D and ARTR2.LD to soils data
april.l <- cbind(d.april.l,s.april) #combine density and soils for april ARTR
usgs.l <- cbind(d.usgs.l,s.usgs) #combine density and soils for usgs ARTR
april.d <- cbind(d.april.d,s.april) #combine density and soils for april ARTR
usgs.d <- cbind(d.usgs.d,s.usgs) #combine density and soils for usgs ARTR
april.ld <- cbind(d.april.ld,s.april) #combine density and soils for april ARTR.LD
usgs.ld <- cbind(d.usgs.ld,s.usgs) #combine density and soils for usgs ARTR.LD

names(usgs.l)[1] <- "ARTR2" # Rename to something meaningful
names(april.l)[1] <- "ARTR2" # Rename to something meaningful
names(usgs.d)[1] <- "ARTR2.D" # Rename to something meaningful
names(april.d)[1] <- "ARTR2.D" # Rename to something meaningful
names(usgs.ld)[1] <- "ARTR2.LD" # Rename to something meaningful
names(april.ld)[1] <- "ARTR2.LD" # Rename to something meaningful


# #Combine SageLive, LPI, and soils
# sageL <- read.csv("F:/ShrubDensity/PresenceAbsence/AprilSageLivePresenceAbsence.csv", row.names=1)
# colnames(sageL) <- c("sage")
# # lpi <- read.csv("F:/LPI/AprilLPIRelativeCoverCommonInExcel.csv")
# # lpi <- lpi[,-1] # remove plot names so don't duplicate below
# soils <- read.csv("F:/Soils/SoilEnvironmentalDataModWithColbyAWS.csv", row.names=1)
# 
# live <- cbind(sageL,soils)


################ Boruta #######################################
## Variable Selection

# set.seed(668)
Boruta.live <- Boruta(ARTR2~., data = april.l, doTrace = 2, ntree = 1000)
Boruta.live
TentativeRoughFix(Boruta.live, averageOver = Inf)

# ten <-TentativeRoughFix(Boruta.live)
# plot(Boruta.live)
# plot(ten)
# getSelectedAttributes(ten, withTentative = T)
# getImpRfZ(april.l,april.l$ARTR2, num.trees = 500)
# 
# plotImpHistory(Boruta.live, colCode = c("green", "yellow", "red", "blue"), col = NULL,
#                type = "l", lty = 1, pch = 0, xlab = "Classifier run",
#                ylab = "Importance")


Boruta.d <- Boruta(ARTR2LD~., data = april.d, doTrace = 2, ntree = 1000)
Boruta.d
TentativeRoughFix(Boruta.d)

# plot(Boruta.d,main="Boruta Live&Dead")
# TentativeRoughFix(Boruta.d)
# TentativeRoughFix(Boruta.d, averageOver = Inf)
# getImpRfGini(april.d,april.d$ARTR2.D, ntree = 500, num.trees = ntree)
# getSelectedAttributes(Boruta.d, withTentative = T)



Boruta.ld <- Boruta(ARTR2.LD~., data = april.ld, doTrace = 2, ntree = 1000)
Boruta.ld
TentativeRoughFix(Boruta.ld)

# plot(Boruta.ld,main="Boruta Live&Dead")
# TentativeRoughFix(Boruta.ld, averageOver = Inf)
# getImpRfGini(april.ld,april.ld$ARTR2LD, ntree = 500, num.trees = ntree)
# getSelectedAttributes(Boruta.ld, withTentative = T)

#Kappa and Class Functions
## Cohen's Kappa is the percent correctly classified corrected
##   by the number correctly classified that you'd expect
##   by chance.

kappa=function(x){
  n=sum(x)
  pobs=(x[1,1]+x[2,2])/n
  pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
  kappa=(pobs-pexp)/(1-pexp)
  t1=0
  t2=0
  t3=0
  pii=x/n
  pidot=apply(pii,1,sum)
  pdotj=apply(pii,2,sum)
  for(i in 1:2){
    t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
  }
  t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + pidot[1])^2
  t3 = (pobs*pexp-2*pexp+pobs)^2
  vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
  se=sqrt(vhat)
  return(c(kappa,se))
}


class.sum=function(truth,predicted){
  xt=table(truth,round(predicted+0.000001))
  pcc=round(100*sum(diag(xt))/sum(xt),2)
  spec=round(100*xt[1,1]/sum(xt[1,]),2)
  sens=round(100*xt[2,2]/sum(xt[2,]),2)
  kap=round(kappa(xt)[1],4)
  au=round(roc.area(truth,predicted)$A,4)
  list(round(c(pcc,spec,sens,kap,au),3))
}

####
#Random Forest
####
# Using only ARTR2 (live)
# Boruta ARTR2 important
# H2_MinDryHue + Depth150 + TotalDepth +
#   Elevation + H1_Effervescence

live.rf = randomForest(as.factor(ARTR2) ~ 
                         Aspect   +
                         BioticCrustClass + Elevation +
                       CarbonateStage +
                         Depth200  +
                       Elevation + SlopeShape
                       , data = april.l,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit, mtry = 2, 
                       ntree = 1000)

live.rf
# live.rf$confusion
# class.sum(l.april$ARTR2,predict(live.rf,type="prob")[,2])

varImpPlot(live.rf, main = 'Live Sagebrush')
   

####
## Crossvalidation
####
 # remove factors that are below 0 on variable importance plot
 #   looking at the Mean Decrease in Accuracy.

live.rf.xval.prob=rep(0,nrow(april.l))
xvs=rep(1:10,length=nrow(april.l))
xvs=sample(xvs)
for(i in 1:10){
  train=april.l[xvs!=i,]
  test=april.l[xvs==i,]
  rf=randomForest(as.factor(ARTR2) ~ 
                    Aspect   +
                    BioticCrustClass + Elevation +
                    CarbonateStage +
                    Depth200  +
                    Elevation + SlopeShape
                    ,data=train)
  live.rf.xval.prob[xvs==i]=predict(rf,test,type="prob")[,2]
}

live.rf.confuse.xval=table(april.l$ARTR2,live.rf.xval.prob)
100-100*sum(diag(live.rf.xval.prob))/nrow(april.l)

table(april.l$ARTR2,round(live.rf.xval.prob+ 0.0000001))
class.sum(april.l$ARTR2,live.rf.xval.prob)

rf
varImpPlot(rf, main = 'Live Sagebrush')


###
# Variable Importance and Partial Dependence Plots
###
varImpPlot(live.rf)

partialPlot(live.rf,l.april, BioticCrustClass, which.class="1", main = 'Live Sagebrush Partial Dependence on Biotic Crust Class')

partialPlot(live.rf,l.april, CarbonateStage, which.class="1", main = 'Live Sagebrush Partial Dependence on CarbonateStage Crust Class')

partialPlot(live.rf,l.april, Depth50, which.class="1", main = 'Live Sagebrush Partial Dependence on Depth50 ')

partialPlot(live.rf,l.april, Depth100, which.class="1", main = 'Live Sagebrush Partial Dependence on Depth100')

partialPlot(live.rf,l.april, Depth150, which.class="1", main = 'Live Sagebrush Partial Dependence on Depth150')

partialPlot(live.rf,l.april, Depth200, which.class="1", main = 'Live Sagebrush Partial Dependence on Depth200')

partialPlot(live.rf,l.april, Elevation, which.class="1", main = 'Live Sagebrush Partial Dependence on Elevation')

# partialPlot(live.rf,live, MaxAWC, which.class="1", main = 'Live Sagebrush Partial Dependence on MaxAWC')

partialPlot(live.rf,l.april, TotalDepth, which.class="1", main = 'Live Sagebrush Partial Dependence on maxDepth')

# partialPlot(live.rf,live, maxDryValue, which.class="1", main = 'Live Sagebrush Partial Dependence on maxDryValue')
# 
# partialPlot(live.rf,live, maxpH, which.class="1", main = 'Live Sagebrush Partial Dependence on maxpH')
# 
# partialPlot(live.rf,live, minDryChroma, which.class="1", main = 'Live Sagebrush Partial Dependence on minDryChroma')
# 
# partialPlot(live.rf,live, minDryValue, which.class="1", main = 'Live Sagebrush Partial Dependence on minDryValue')
# 
# partialPlot(live.rf,live, minMoistChroma, which.class="1", main = 'Live Sagebrush Partial Dependence on minMoistChroma')
# 
# partialPlot(live.rf,live, minMoistValue, which.class="1", main = 'Live Sagebrush Partial Dependence on minMoistValue')
# 
partialPlot(live.rf,l.april, SlopeShape, which.class="1", main = 'Live Sagebrush Partial Dependence on SlopeShape')



####
# Random Forests for Important Variable Only
####

live.rf.imp=randomForest(as.factor(ARTR2)~  
                           Depth150 +Depth100 + TotalDepth +
                           H2_MinDryChroma + Elevation +
                           H1_DryChroma
                       ,proximity=TRUE,importance=TRUE,keep.forest=TRUE,data=l.april)
# live.rf.imp$confusion
# class.sum(live$sage,predict(live.rf.imp,type="prob")[,2])
# 
# live.rf.confuse.xval=table(live$sage,live.rf.xval.prob)
# 100-100*sum(diag(live.rf.xval.prob))/nrow(live)
live.rf.imp
varImpPlot(live.rf.imp)

####
## Crossvalidation for Important Variables
####

live.rf.imp.xval.prob=rep(0,nrow(l.april))
xvs=rep(1:10,length=nrow(l.april))
xvs=sample(xvs)
for(i in 1:10){
  train=l.april[xvs!=i,]
  test=l.april[xvs==i,]
  rf=randomForest(as.factor(ARTR2)~  
                    Depth150 +Depth100 + TotalDepth +
                    H2_MinDryChroma + Elevation +
                    H1_DryChroma
                  ,data=train)
  live.rf.imp.xval.prob[xvs==i]=predict(rf,test,type="prob")[,2]
}

# table(live$sage,round(live.rf.imp.xval.prob+ 0.0000001))
# class.sum(live$sage,live.rf.imp.xval.prob)
# 
# live.rf.confuse.xval=table(live$sage,live.rf.imp.xval.prob)
# 100-100*sum(diag(live.rf.imp.xval.prob))/nrow(live)
# 
rf

## Interpretation ##

##                Predicted
#             Absence   Presence
## Actual
# Absence       a           b
# Presence      c           d

# 1.PCC 2.Specificity 3.Sensitivity 4.Kappa 5.AUC

# AUC = area underneath the ROC curve
# 0.5 <= AUC <= 1
# Good value of AUC 0.95

# K < 0 Worse than chance. Really bad
# K 0<= K <= 0.3 Poor
# K 0.3 <= K <= 0.7 Moderate
# K 0.7 Good classification

# PCC = percentage correctly classified 100% * ((a+d)/n)
# Overall Error Rate = 100% - PCC
# Specificity = percentage of absences correctly classified 100% * (a/(a+b) 
# Error Rate for Absences = 100% - Specificity = 100% * (b/(a+b))
# Sensitivity = percent of presences correctly classified = 100% * (d/(c+d))
# Error Rate for Presences = 100% - Sensitivity = 100% * (c/(c+d))

