####
#Classification Tree
####
# Using only ARTR (live)

#The prp() has a lot of different options to check out
#prp(model)

#This one has the best default
#fancyRpartPlot(model)

# Grow Full Tree
full_tree = rpart(as.factor(sage) ~ 
                    AWC100 + AWC25 + AWC50 + BioticCrustClass +
                    CarbonateStage + Depth100 + Depth150 + Depth200 +
                    Depth50 + Elevation + MaxAWC + maxClay + maxSand +
                    maxDepth + maxDryChroma + maxDryValue + minClay +
                    maxMoistChroma + maxMoistValue + maxpH + minpH +
                    minDryChroma + minDryValue + minMoistChroma +
                    minMoistValue + minSand + SlopeShape +Subsurface +
                    Surface + TotalAWC
                  , data = live, control = 
                    rpart.control(cp = 0.0, minsplit = 2))
plotcp(full_tree) #Use first drop below SE to determine cp cutoff.

# Prune the Tree
# Nodes = 3, cp = 0.14
final_tree = rpart(as.factor(sage) ~ 
                     AWC100 + AWC25 + AWC50 + BioticCrustClass +
                     CarbonateStage + Depth100 + Depth150 + Depth200 +
                     Depth50 + Elevation + MaxAWC + maxClay + maxSand +
                     maxDepth + maxDryChroma + maxDryValue + minClay +
                     maxMoistChroma + maxMoistValue + maxpH + minpH +
                     minDryChroma + minDryValue + minMoistChroma +
                     minMoistValue + minSand + SlopeShape +Subsurface +
                     Surface + TotalAWC
                   , data = live, control = 
                     rpart.control(cp = 0.07, minsplit = 2))

plot(final_tree, margin=.3)
text(final_tree,use.n=TRUE)

final_tree.confuse=table(live$sage,predict(final_tree,type="class"))
100-100*sum(diag(final_tree.confuse))/nrow(live)

#The prp() has a lot of different options to check out
# extra = 3 gives misclassified/total in node
# under = T puts text under box
prp(final_tree, extra = 3, under = T, varlen = 0, faclen = 0, main = 'Live Sagebrush')

#This one has the best default
fancyRpartPlot(final_tree)

####
#Crossvalidation Classification Tree
####
# Using only ARTR (live)

final_tree.xval=rep(0,nrow(live))
xvs=rep(1:10,length=nrow(live))
xvs=sample(xvs)
for(i in 1:10){
  test=live[xvs==i,]
  train=live[xvs!=i,]
  glub= rpart(as.factor(sage) ~ 
                AWC100 + AWC25 + AWC50 + BioticCrustClass +
                CarbonateStage + Depth100 + Depth150 + Depth200 +
                Depth50 + Elevation + MaxAWC + maxClay + maxSand +
                maxDepth + maxDryChroma + maxDryValue + minClay +
                maxMoistChroma + maxMoistValue + maxpH + minpH +
                minDryChroma + minDryValue + minMoistChroma +
                minMoistValue + minSand + SlopeShape +Subsurface +
                Surface + TotalAWC
              , data = train, control = 
                rpart.control(cp = 0.14, minsplit = 2))
  final_tree.xval[xvs==i]=predict(glub,test,type="class")
}

final_tree.confuse.xval=table(live$sage,final_tree.xval)
100-100*sum(diag(final_tree.confuse.xval))/nrow(live)
## Overall xval error rate is around 25%

table(live$sage,round(final_tree.xval+ 0.0000001))
class.sum(live$sage,final_tree.xval)
