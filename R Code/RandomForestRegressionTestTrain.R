#load the data
lpi <- read.csv("F:/LPI/Output/USGSLPIplotXspp.csv",header=TRUE, row.names=1)
soil <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
  # Remove Predetermined Variables (Covariance and VIF)
  soil <- subset(soil, select = -c(DepthClass,Aspect,Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,H1.Texture,SlopeShape,Tot.SandSize,H1.SandSize,H1.DryRed,H1.DryValue,H1.DryCClass,Tot.DryRed,Tot.DryValue,Tot.DryCClass,MaxSand,MaxpH,MaxDryValue,MaxAWHC))
 # soil <- subset(soil, select = -c(Tot.EfferScale,Tot.MoistCClass,Tot.MoistRed,Tot.MoistValue,Slope,BioticCrustClass,MaxEffervescence,H1.MoistCClass,H1.MoistRed,H1.MoistValue))
  # Which rows have NA present somewhere
  rownames(soil)[rowSums(is.na(soil)) > 0]
  # Replace NA with 0 (necesary for Boruta but not Random Forest)
  soil[is.na(soil)] <- 0 # replace NA with 0


# Combine Plant Data and Soils Data
sl <- soil
sl$ARTR2 <- lpi$ARTR2

# this data has 136 rows
nrow(sl)

# look at the first few
head(sl)

# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(sl, seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 68 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset


########### Optional: apply to  data using randomForest ###########

#load the randomForest library. if you havent installed it, run the next line
#install.packages("randomForest")
library(randomForest)

#fit the randomforest model
model <- randomForest(as.numeric(ARTR2) ~ .
                      , data = training,proximity=TRUE,
                      importance=TRUE,keep.forest=TRUE,
                      na.action = na.omit,mtry=2,
                      ntree = 500)
print(model)



#what are the important variables (via permutation)
varImpPlot(model, type=1)

#predict the outcome of the testing data
predicted <- predict(model, newdata=testing[ ,-23])

# what is the proportion variation explained in the outcome of the testing data?
# i.e., what is 1-(SSerror/SStotal)
actual <- testing$ARTR2
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)



# live.rf.xval.prob=rep(0,nrow(sl))
# xvs=rep(1:10,length=nrow(sl))
# xvs=sample(xvs)
# for(i in 1:10){
#   train=sl[xvs!=i,]
#   test=sl[xvs==i,]
#   rf=randomForest(as.factor(ARTR2) ~ .
#                   ,data=train)
#   live.rf.xval.prob[xvs==i]=predict(rf,test,type="prob")[,2]
# }
# 
# # % resubstitution error rate
# live.rf.confuse.xval=table(sl$ARTR2,live.rf.xval.prob)
# 100-100*sum(diag(live.rf.xval.prob))/nrow(sl)
# 
# table(sl$ARTR2,round(live.rf.xval.prob+ 0.0000001))
# class.sum(sl$ARTR2,live.rf.xval.prob)
# 
# rf
# varImpPlot(rf, main = 'Live Sagebrush')
