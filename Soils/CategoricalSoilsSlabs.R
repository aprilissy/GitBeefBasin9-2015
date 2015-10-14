library(splitstackshape)
library(plyr)
library(aqp)
library(data.table)
library(reshape2)
library(tidyr)

# Read in data
# Manually in excel I added in second IL1_9 row of NA to dat so H2 still has 99obs
dat <- read.csv("F:/Soils/SoilDataFitUSGSColumns.csv", header = T,nrows = 444)
udat <- read.csv("F:/Soils/USGSsoildataModForAprilsdata.csv", header = T,nrows = 444)
udat$id <- extract_numeric(udat$id) # removes CLHS and P leaving only numbers.


# Creates an ordered list of each horizon in a plot
dat <- getanID(data = dat, id.vars = "id") 
udat <- getanID(data = udat, id.vars = "id")

H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
Plot <- subset(H1, select = c(id,Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))# Pull out data that is for the whole plot
H1 <- subset(H1, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))# Remove out data that is for the whole plot
dat <- subset(dat, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))# Remove out data that is for the whole plot

# Combine April and USGS soils data
dat <- rbind(dat,udat)

# Add in USGS Elevation, Slope Shape, Slope, Carbonate Stage, Biotic Crust Class
# Remove BLM Trend and Miller plots
site <- read.csv("F:/BeefBasin Data For April/BeefBasin/formattedR/Site_Data.csv")
site <- site[-c(66:77),]
site <- site[order(site$pedonID),] # Sort so plot 100 is by 9 same as data
site$pedonID <- extract_numeric(site$pedonID) # removes CLHS and P leaving only numbers.

loc <- read.csv("F:/BeefBasin Data For April/BeefBasin/formattedR/locInfo.csv")
loc <- loc[-c(66:77),]
loc <- loc[order(loc$Plot.Name),] # Sort so plot 100 is by 9 same as data
loc$Plot.Name <- extract_numeric(loc$Plot.Name) # removes CLHS and P leaving only numbers.

# put into dataframe to add to april
id <-site$pedonID
Elevation <- loc$altitude
Aspect <- site$Aspect
df <- data.frame(id,Elevation,Aspect)
df$Slope <- site$Slope
df$SlopeShape <- site$SlopeShape
df$CarbonateStage <- site$CarbonateStage
df$BioticCrustClass <- site$BioticCrustClass
# Add to april Plot dataframe
Plot <- rbind(Plot,df)

# combine redundant categories
{Plot$SlopeShape <- sub("LC", "CL", Plot$SlopeShape, ignore.case = FALSE)
 Plot$SlopeShape <- sub("VC", "CV", Plot$SlopeShape, ignore.case = FALSE)
 Plot$SlopeShape <- sub("VL", "LV", Plot$SlopeShape, ignore.case = FALSE)
 Plot$SlopeShape <- sub("LVQ", "LV", Plot$SlopeShape, ignore.case = FALSE)
 Plot$SlopeShape <- as.factor(Plot$SlopeShape)}

# get the maximum depth of the entire pedon
PedonDepth <- ddply( dat, .(id), function(x) max(x$bottom, na.rm = T) )
names(PedonDepth)[2] <- 'PedonDepth'

# get the depth of each horizon
dat$Depth <- dat$bottom-dat$top

#  Create Soil Depth Classes
# Very Shallow <25cm - Shallow 25-50cm - Moderately Deep 50-100cm - Deep 100-150cm - Very Deep >150cm
DepthClass <- PedonDepth$PedonDepth
mysize <- function(x){
  if(x<25)return('VeryShallow')
  if(25<= x & x <50)return('Shallow')
  if(50<=x & x<100)return("ModeratelyDeep")
  if(100<=x & x<150)return('Deep')
  if(x>=150) return('VeryDeep')
  elsereturn(NA)}
Plot$DepthClass <- sapply(PedonDepth$PedonDepth, mysize)

# Scale Hue - Redness Scale - Degree of Redness: least(1) to most(4) red. 2.5YR=4, 5YR=3, 7.5YR=2, 10YR=1.
# Scale Chroma - Chroma Class: 1&2=Low, 3&4=Medium, 6&8=High
dat$DryH <- dat$DryHue
dat$MoistH <- dat$MoistHue
dat$DryChroma <- sub("5", "4", dat$DryChroma, ignore.case = FALSE)
dat$DryC <- dat$DryChroma
dat$MoistC <- dat$MoistChroma
dat$Effervescence <- sub("LS", "SL", dat$Effervescence, ignore.case = FALSE)
dat$Effer <- dat$Effervescence

{
  dat$DryHue <- sub("2.5YR", "4", dat$DryHue, ignore.case = FALSE)
  dat$DryHue <- sub("7.5YR", "2", dat$DryHue, ignore.case = FALSE)
  dat$DryHue <- sub("5YR", "3", dat$DryHue, ignore.case = FALSE)
  dat$DryHue <- sub("10YR", "1", dat$DryHue, ignore.case = FALSE)
  
  dat$MoistHue <- sub("2.5YR", "4", dat$MoistHue, ignore.case = FALSE)
  dat$MoistHue <- sub("7.5YR", "2", dat$MoistHue, ignore.case = FALSE)
  dat$MoistHue <- sub("5YR", "3", dat$MoistHue, ignore.case = FALSE)
  dat$MoistHue <- sub("10YR", "1", dat$MoistHue, ignore.case = FALSE)
  
  dat$DryChroma <- sub("2", "1", dat$DryChroma, ignore.case = FALSE)
  dat$DryChroma <- sub("3", "2", dat$DryChroma, ignore.case = FALSE)
  dat$DryChroma <- sub("4", "2", dat$DryChroma, ignore.case = FALSE)
  dat$DryChroma <- sub("6", "3", dat$DryChroma, ignore.case = FALSE)
  dat$DryChroma <- sub("8", "3", dat$DryChroma, ignore.case = FALSE)
  
  
  dat$MoistChroma <- sub("2", "1", dat$MoistChroma, ignore.case = FALSE)
  dat$MoistChroma <- sub("3", "2", dat$MoistChroma, ignore.case = FALSE)
  dat$MoistChroma <- sub("4", "2", dat$MoistChroma, ignore.case = FALSE)
  dat$MoistChroma <- sub("6", "3", dat$MoistChroma, ignore.case = FALSE)
  
  dat$Effervescence <- sub("VE", "4", dat$Effervescence, ignore.case = FALSE)
  dat$Effervescence <- sub("ST", "3", dat$Effervescence, ignore.case = FALSE)
  dat$Effervescence <- sub("SL", "2", dat$Effervescence, ignore.case = FALSE)
  dat$Effervescence <- sub("VS", "1", dat$Effervescence, ignore.case = FALSE)
  dat$Effervescence <- sub("NE", "0", dat$Effervescence, ignore.case = FALSE)
   
  dat$Texture <- sub("LVFS", "LS", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("VFLS", "LS", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("VFSL", "SL", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("FSL", "SL", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("LCS", "LS", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("LM", "L", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("FLS", "LS", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("FS", "S", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("LFS", "LS", dat$Texture, ignore.case = FALSE)
  dat$Texture <- sub("LMS", "LS", dat$Texture, ignore.case = FALSE)

  
  dat$SandSize <- sub("MID", "MIX", dat$SandSize, ignore.case = FALSE)
  dat$SandSize <- sub("Vf, M", "MIX", dat$SandSize, ignore.case = FALSE)
  dat$SandSize <- sub("VFS", "VF", dat$SandSize, ignore.case = FALSE)
  dat$SandSize <- sub("VF ", "VF", dat$SandSize, ignore.case = FALSE)

}
dat <- rename(dat, c(DryHue="DryRed",DryH="DryHue",MoistHue="MoistRed",
             MoistH="MoistHue",DryChroma="DryCClass",DryC="DryChroma",
             MoistChroma="MoistCClass",MoistC="MoistChroma",
             Effervescence="EfferScale",Effer="Effervescence"))


# write file before removing anything for use in slabs function below
write.csv(dat,file="F:/Soils/SoilDataAprilUSGSnotremoved.csv", row.names=FALSE)
# # remove anything you don't want duplicated in H1 and H2
# dat <- subset(dat, select = -c(top,bottom,Horizon,Theta_fc,Theta_pwp, HzNum,Texture,SandSize) )

H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
H1 <- subset(H1, select=-c(.id))
H1$DWA_AWC <- (H1$AWHC*H1$Depth)/PedonDepth$PedonDepth

H2 <- dat[! which(dat$.id=='1'), ] # Pull out horizon #1
H2 <- subset(H2, select=-c(.id))

{
# Categorical Depth Weighted Mode

  # Total Horizon
TDWA <- PedonDepth

TDryRed <- count(dat, vars=c("DryRed", "id"),wt_var="Depth")
TDryRed<-xtabs(freq~id+DryRed, TDryRed)
TDWA$DryRed <- colnames(TDryRed)[apply(TDryRed,1,which.max)]

TDryHue <- count(dat, vars=c("DryHue", "id"),wt_var="Depth")
TDryHue<-xtabs(freq~id+DryHue, TDryHue)
TDWA$DryHue <- colnames(TDryHue)[apply(TDryHue,1,which.max)]

TDryValue <- count(dat, vars=c("DryValue", "id"),wt_var="Depth")
TDryValue<-xtabs(freq~id+DryValue, TDryValue)
TDWA$DryValue <- colnames(TDryValue)[apply(TDryValue,1,which.max)]

TDryCClass <- count(dat, vars=c("DryCClass", "id"),wt_var="Depth")
TDryCClass<-xtabs(freq~id+DryCClass, TDryCClass)
TDWA$DryCClass <- colnames(TDryCClass)[apply(TDryCClass,1,which.max)]
TDWA[c(52,60,63),6]=3 # fix 3 occurences where chroma 6 was class 2

TDryChroma <- count(dat, vars=c("DryChroma", "id"),wt_var="Depth")
TDryChroma<-xtabs(freq~id+DryChroma, TDryChroma)
TDWA$DryChroma <- colnames(TDryChroma)[apply(TDryChroma,1,which.max)]

TMoistRed <- count(dat, vars=c("MoistRed", "id"),wt_var="Depth")
TMoistRed<-xtabs(freq~id+MoistRed, TMoistRed)
TDWA$MoistRed <- colnames(TMoistRed)[apply(TMoistRed,1,which.max)]

TMoistHue <- count(dat, vars=c("MoistHue", "id"),wt_var="Depth")
TMoistHue<-xtabs(freq~id+MoistHue, TMoistHue)
TDWA$MoistHue <- colnames(TMoistHue)[apply(TMoistHue,1,which.max)]

TMoistValue <- count(dat, vars=c("MoistValue", "id"),wt_var="Depth")
TMoistValue<-xtabs(freq~id+MoistValue, TMoistValue)
TDWA$MoistValue <- colnames(TMoistValue)[apply(TMoistValue,1,which.max)]

TMoistCClass <- count(dat, vars=c("MoistCClass", "id"),wt_var="Depth")
TMoistCClass<-xtabs(freq~id+MoistCClass, TMoistCClass)
TDWA$MoistCClass <- colnames(TMoistCClass)[apply(TMoistCClass,1,which.max)]
TDWA[c(105,154),11]=3 # fix 1 occurence where chroma 6 was class 2

TMoistChroma <- count(dat, vars=c("MoistChroma", "id"),wt_var="Depth")
TMoistChroma<-xtabs(freq~id+MoistChroma, TMoistChroma)
TDWA$MoistChroma <- colnames(TMoistChroma)[apply(TMoistChroma,1,which.max)]

TTexture <- count(dat, vars=c("Texture", "id"),wt_var="Depth")
TTexture<-xtabs(freq~id+Texture, TTexture)
TDWA$Texture <- colnames(TTexture)[apply(TTexture,1,which.max)]

TSandSize <- count(dat, vars=c("SandSize", "id"),wt_var="Depth")
TSandSize<-xtabs(freq~id+SandSize, TSandSize)
TDWA$SandSize <- colnames(TSandSize)[apply(TSandSize,1,which.max)]

TEffervescence <- count(dat, vars=c("Effervescence", "id"),wt_var="Depth")
TEffervescence<-xtabs(freq~id+Effervescence, TEffervescence)
TDWA$Effervescence <- colnames(TEffervescence)[apply(TEffervescence,1,which.max)]

TEfferScale <- count(dat, vars=c("EfferScale", "id"),wt_var="Depth")
TEfferScale<-xtabs(freq~id+EfferScale, TEfferScale)
TDWA$EfferScale <- colnames(TEfferScale)[apply(TEfferScale,1,which.max)]

  # Subsurface Horizon
SDWA <- PedonDepth

SDryRed <- count(H2, vars=c("DryRed", "id"),wt_var="Depth")
SDryRed<-xtabs(freq~id+DryRed, SDryRed)
SDWA$DryRed <- colnames(SDryRed)[apply(SDryRed,1,which.max)]

SDryHue <- count(H2, vars=c("DryHue", "id"),wt_var="Depth")
SDryHue<-xtabs(freq~id+DryHue, SDryHue)
SDWA$DryHue <- colnames(SDryHue)[apply(SDryHue,1,which.max)]

SDryValue <- count(H2, vars=c("DryValue", "id"),wt_var="Depth")
SDryValue<-xtabs(freq~id+DryValue, SDryValue)
SDWA$DryValue <- colnames(SDryValue)[apply(SDryValue,1,which.max)]

SDryCClass <- count(H2, vars=c("DryCClass", "id"),wt_var="Depth")
SDryCClass<-xtabs(freq~id+DryCClass, SDryCClass)
SDWA$DryCClass <- colnames(SDryCClass)[apply(SDryCClass,1,which.max)]
SDWA[c(52),6]=3 # fix 3 occurences where chroma 6 was class 2

SDryChroma <- count(H2, vars=c("DryChroma", "id"),wt_var="Depth")
SDryChroma<-xtabs(freq~id+DryChroma, SDryChroma)
SDWA$DryChroma <- colnames(SDryChroma)[apply(SDryChroma,1,which.max)]

SMoistRed <- count(H2, vars=c("MoistRed", "id"),wt_var="Depth")
SMoistRed<-xtabs(freq~id+MoistRed, SMoistRed)
SDWA$MoistRed <- colnames(SMoistRed)[apply(SMoistRed,1,which.max)]

SMoistHue <- count(H2, vars=c("MoistHue", "id"),wt_var="Depth")
SMoistHue<-xtabs(freq~id+MoistHue, SMoistHue)
SDWA$MoistHue <- colnames(SMoistHue)[apply(SMoistHue,1,which.max)]

SMoistValue <- count(H2, vars=c("MoistValue", "id"),wt_var="Depth")
SMoistValue<-xtabs(freq~id+MoistValue, SMoistValue)
SDWA$MoistValue <- colnames(SMoistValue)[apply(SMoistValue,1,which.max)]

SMoistCClass <- count(H2, vars=c("MoistCClass", "id"),wt_var="Depth")
SMoistCClass<-xtabs(freq~id+MoistCClass, SMoistCClass)
SDWA$MoistCClass <- colnames(SMoistCClass)[apply(SMoistCClass,1,which.max)]
SDWA[c(105,154),11]=3 # fix 3 occurences where chroma 6 was class 2

SMoistChroma <- count(H2, vars=c("MoistChroma", "id"),wt_var="Depth")
SMoistChroma<-xtabs(freq~id+MoistChroma, SMoistChroma)
SDWA$MoistChroma <- colnames(SMoistChroma)[apply(SMoistChroma,1,which.max)]

STexture <- count(H2, vars=c("Texture", "id"),wt_var="Depth")
STexture<-xtabs(freq~id+Texture, STexture)
SDWA$Texture <- colnames(STexture)[apply(STexture,1,which.max)]

SSandSize <- count(H2, vars=c("SandSize", "id"),wt_var="Depth")
SSandSize<-xtabs(freq~id+SandSize, SSandSize)
SDWA$SandSize <- colnames(SSandSize)[apply(SSandSize,1,which.max)]

SEffervescence <- count(H2, vars=c("Effervescence", "id"),wt_var="Depth")
SEffervescence<-xtabs(freq~id+Effervescence, SEffervescence)
SDWA$Effervescence <- colnames(SEffervescence)[apply(SEffervescence,1,which.max)]

SEfferScale <- count(H2, vars=c("EfferScale", "id"),wt_var="Depth")
SEfferScale<-xtabs(freq~id+EfferScale, SEfferScale)
SDWA$EfferScale <- colnames(SEfferScale)[apply(SEfferScale,1,which.max)]

}

MaxClay <- ddply( H2, 'id', summarize, MaxClay = max(ClayPercent, na.rm = T))
MaxSand <- ddply(H2, 'id', summarize, MaxSand = max(SandPercent, na.rm = T))
MaxpH <- ddply(H2, 'id', summarize, MaxpH = max(pH, na.rm = T))
MaxDryValue <- ddply(H2, 'id', summarize, MaxDryValue = max(DryValue, na.rm = T))
MaxAWHC <- ddply( H2, 'id', summarize, MaxAWHC = max(AWHC, na.rm = T))
MaxEffervescence <- ddply( H2, 'id', summarize, MaxEffervescence = max(EfferScale, na.rm = T))

Max <- join(MaxClay, MaxSand, by = 'id', type = 'inner')
Max <- join(Max, MaxpH, by = 'id', type = 'inner')
Max <- join(Max, MaxDryValue, by = 'id', type = 'inner')
Max <- join(Max, MaxAWHC, by = 'id', type = 'inner')
Max <- join(Max, MaxEffervescence, by = 'id', type = 'inner')
is.na(Max) <- sapply(Max, is.infinite) # replace inf- with NA


#Now calculate depth weighted averages of each continuous variable, then append these to the other variables. 
#Convert to SoilProfileCollection
data <- read.csv("F:/Soils/SoilDataAprilUSGSnotremoved.csv", header = T)
depths(data) <- id ~ top + bottom

# # within each profile, compute weighted means, over the intervals: 0-25,0-50,0-100,0-150,0-200 removing NA if present 
# d25 <- slab(data, id ~ AWHC, slab.structure = c(0,25), slab.fun = mean, na.rm=TRUE)
# d50 <- slab(data, id ~ AWHC, slab.structure = c(0,50), slab.fun = mean, na.rm=TRUE)
# d100 <- slab(data, id ~ AWHC, slab.structure = c(0,100), slab.fun = mean, na.rm=TRUE)
# d150 <- slab(data, id ~ AWHC, slab.structure = c(0,150), slab.fun = mean, na.rm=TRUE)
# d200 <- slab(data, id ~ AWHC, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
# 
# 
# s50 <- slab(data, id ~ AWHC, slab.structure = c(25,50), slab.fun = mean, na.rm=TRUE)
# s100 <- slab(data, id ~ AWHC, slab.structure = c(50,100), slab.fun = mean, na.rm=TRUE)
# s150 <- slab(data, id ~ AWHC, slab.structure = c(100,150), slab.fun = mean, na.rm=TRUE)
# s200 <- slab(data, id ~ AWHC, slab.structure = c(150,200), slab.fun = mean, na.rm=TRUE)
# 
# 
# # reshape to wide format, remove unneeded variables and rename. 
# AWC.0.25 <- dcast(d25, id + top + bottom ~ variable, value.var = 'value')
# AWC.0.25 <- AWC.0.25[,-c(2,3)]
# names(AWC.0.25)[2] <- 'AWC.0.25'
# 
# AWC.0.50 <- dcast(d50, id + top + bottom ~ variable, value.var = 'value')
# AWC.0.50 <- AWC.0.50[,-c(2,3)]
# names(AWC.0.50)[2] <- 'AWC.0.50'
# 
# AWC.0.100 <- dcast(d100, id + top + bottom ~ variable, value.var = 'value')
# AWC.0.100 <- AWC.0.100[,-c(2,3)]
# names(AWC.0.100)[2] <- 'AWC.0.100'
# 
# AWC.0.150 <- dcast(d150, id + top + bottom ~ variable, value.var = 'value')
# AWC.0.150 <- AWC.0.150[,-c(2,3)]
# names(AWC.0.150)[2] <- 'AWC.0.150'
# 
# AWC.0.200 <- dcast(d200, id + top + bottom ~ variable, value.var = 'value')
# AWC.0.200 <- AWC.0.200[,-c(2,3)]
# names(AWC.0.200)[2] <- 'AWC.0.200'
# 
# AWC.25.50 <- dcast(s50, id + top + bottom ~ variable, value.var = 'value')
# AWC.25.50 <- AWC.25.50[,-c(2,3)]
# names(AWC.25.50)[2] <- 'AWC.25.50'
# 
# AWC.50.100 <- dcast(s100, id + top + bottom ~ variable, value.var = 'value')
# AWC.50.100 <- AWC.50.100[,-c(2,3)]
# names(AWC.50.100)[2] <- 'AWC.50.100'
# 
# AWC.100.150 <- dcast(s150, id + top + bottom ~ variable, value.var = 'value')
# AWC.100.150 <- AWC.100.150[,-c(2,3)]
# names(AWC.100.150)[2] <- 'AWC.100.150'
# 
# AWC.150.200 <- dcast(s200, id + top + bottom ~ variable, value.var = 'value')
# AWC.150.200 <- AWC.150.200[,-c(2,3)]
# names(AWC.150.200)[2] <- 'AWC.150.200'


# within each profile, compute weighted means, removing NA if present 
dwaclay <- slab(data, id ~ ClayPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
dwasand <- slab(data, id ~ SandPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
dwapH <- slab(data, id ~ pH, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
dwaawhc <- slab(data, id ~ AWHC, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)



# reshape to wide format, remove unneeded variables and rename. 
DWAClay <- dcast(dwaclay, id + top + bottom ~ variable, value.var = 'value')
DWAClay <- DWAClay[,-c(2,3)]
names(DWAClay)[2] <- 'DWAClay'

DWASand <- dcast(dwasand, id + top + bottom ~ variable, value.var = 'value')
DWASand <- DWASand[,-c(2,3)]
names(DWASand)[2] <- 'DWASand'

DWApH <- dcast(dwapH, id + top + bottom ~ variable, value.var = 'value')
DWApH <- DWApH[,-c(2,3)]
names(DWApH)[2] <- 'DWApH'

DWA.AWHC <- dcast(dwaawhc, id + top + bottom ~ variable, value.var = 'value')
DWA.AWHC <- DWA.AWHC[,-c(2,3)]
names(DWA.AWHC)[2] <- 'DWA.AWHC'

# #Now calculate depth weighted averages of H2
# #Convert to SoilProfileCollection
# Sub <- H2
# depths(Sub) <- id ~ top + bottom
# 
# # within each profile, compute weighted means, removing NA if present 
# subdwaclay <- slab(Sub, id ~ ClayPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
# subdwasand <- slab(Sub, id ~ SandPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
# subdwapH <- slab(Sub, id ~ pH, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
# subawhc <- slab(Sub, id ~ AWHC, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
# 
# # reshape to wide format, remove unneeded variables and rename. 
# SubDWAClay <- dcast(subdwaclay, id + top + bottom ~ variable, value.var = 'value')
# SubDWAClay <- SubDWAClay[,-c(2,3)]
# names(SubDWAClay)[2] <- 'SubDWAClay'
# 
# SubDWASand <- dcast(subdwasand, id + top + bottom ~ variable, value.var = 'value')
# SubDWASand <- SubDWASand[,-c(2,3)]
# names(SubDWASand)[2] <- 'SubDWASand'
# 
# SubDWApH <- dcast(subdwapH, id + top + bottom ~ variable, value.var = 'value')
# SubDWApH <- SubDWApH[,-c(2,3)]
# names(SubDWApH)[2] <- 'SubDWApH'
# 
# SubAWC <- dcast(subawhc, id + top + bottom ~ variable, value.var = 'value')
# SubAWC <- SubAWC[,-c(2,3)]
# names(SubAWC)[2] <- 'SubAWC'


# slabs <- join(AWC.0.25, AWC.0.50, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.0.100, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.0.150, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.0.200, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.25.50, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.50.100, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.100.150, by = 'id', type = 'inner')
# slabs <- join(slabs, AWC.150.200, by = 'id', type = 'inner')

slabs <- join(DWAClay, DWASand, by = 'id', type = 'inner')
slabs <- join(slabs, DWApH, by = 'id', type = 'inner')
slabs <- join(slabs, DWA.AWHC, by = 'id', type = 'inner')
# slabs <- join(slabs, SubDWAClay, by = 'id', type = 'inner')
# slabs <- join(slabs, SubDWASand, by = 'id', type = 'inner')
# slabs <- join(slabs, SubDWApH, by = 'id', type = 'inner')
# slabs <- join(slabs, SubAWC, by = 'id', type = 'inner')

Plot <- join(Plot, PedonDepth, by = 'id', type = 'inner')

# remove anything you don't want H1, H2, Plot, TDWA, SDWA, slabs
H1 <- subset(H1, select = -c(top,bottom,Horizon,Theta_fc,Theta_pwp,AWHC, HzNum) )
SDWA <- subset(SDWA, select = -c(PedonDepth) )
TDWA <- subset(TDWA, select = -c(PedonDepth) )

colnames(H1) = paste("H1", sep=".", colnames(H1)) # Rename variables for H1
H1 <- rename(H1, c("H1.id"="id")) 

colnames(TDWA) = paste("Tot",sep=".", colnames(TDWA)) # Rename variables for H1
names(TDWA)[names(TDWA)=="Tot.id"]<-"id"
colnames(SDWA) = paste("Sub",sep=".", colnames(SDWA)) # Rename variables for H1
names(SDWA)[names(SDWA)=="Sub.id"]<-"id"

# # Create new soil parameter where depth is binary.
# # if the maximum depth is >50/100/150 then 1, if not then 0
# all$Depth50 <- as.numeric(all$PedonDepth > 50)
# all$Depth100 <- as.numeric(all$PedonDepth > 100)
# all$Depth150 <- as.numeric(all$PedonDepth > 150)
Plot$Depth200 <- as.numeric(Plot$PedonDepth == 200)

H1 <- subset(H1, select = -c(H1.DryHue,H1.MoistHue,H1.DryChroma,H1.MoistChroma,H1.Effervescence) )
TDWA <- subset(TDWA, select = -c(Tot.DryHue,Tot.MoistHue,Tot.DryChroma,Tot.MoistChroma,Tot.Effervescence) )
SDWA <- subset(SDWA, select = -c(Sub.DryHue,Sub.MoistHue,Sub.DryChroma,Sub.MoistChroma,Sub.Effervescence) )


Soils <- merge(Plot,H1,by='id')
Soils <- merge(Soils,TDWA,by='id')
# Soils <- merge(Soils,SDWA,by='id')
Soils <- merge(Soils,slabs,by='id')
Soils <- merge(Soils,Max,by='id')

#####
# Keep only Soils data that has matching veg data.
# Add to April Soils
SoilstoKeep <- Soils[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),]
USGSinNSplain <- Soils[c("24","38","40","42","43","80","82"),]
April1 <- Soils[c(66:164),]
Veg <- rbind(April1, SoilstoKeep)
NSveg <- rbind(April1,USGSinNSplain)

# USGS & April
write.csv(Veg,file="F:/Soils/SoilEnvironmentaldataUSGSApril.csv", row.names=FALSE)
# USGS in N&S plain & April
write.csv(NSveg,file="F:/Soils/SoilEnvironmentaldataNSplain.csv", row.names=FALSE)
# April
write.csv(April1,file="F:/Soils/SoilEnvironmentaldataApril.csv", row.names=FALSE)
