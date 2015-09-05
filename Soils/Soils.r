#April's Soil Data
#install.packages('plyr')
library(plyr)
library(data.table)
library(splitstackshape)

# Read in Soils Data
dat <- read.csv("F:/Soils/SoilDataInputR.csv", header = T,nrows = 444)
# Pull out meaningful factors
df1 <- dat[,c(1:3,5:10,17,19,20,36,39:40,46,54)]
df2 <- dat[,c(1,40)] # Slope shape
SlopeShape <- df2[complete.cases(df2), ] #Remove rows with only NA
# Combine Slope Shape categories
SlopeShape$SlopeShape <- sub("LC", "CL", SlopeShape$SlopeShape, ignore.case = FALSE)
SlopeShape$SlopeShape <- sub("VC", "CV", SlopeShape$SlopeShape, ignore.case = FALSE)
SlopeShape$SlopeShape <- sub("VL", "LV", SlopeShape$SlopeShape, ignore.case = FALSE)

df1$Elevation <- df1$Elevation/3.2808 # Change Elevation from ft to m

#Numeric Data
dat$top <- as.numeric(dat$top)
dat$bottom <- as.numeric(dat$bottom)
dat$ClayPercent <- as.numeric(dat$ClayPercent)
dat$Elevation <- as.numeric(dat$Elevation)
dat$SandPercent <- as.numeric(dat$SandPercent)
dat$pH <- as.numeric(dat$pH)
dat$Slope <- as.numeric(dat$Slope)
# Factor Data
dat$SlopeShape <- as.factor(dat$SlopeShape)
dat$DryValue <- as.factor(dat$DryValue)
dat$DryChroma <- as.factor(dat$DryChroma)
dat$MoistValue <- as.factor(dat$MoistValue)
dat$MoistChroma <- as.factor(dat$MoistChroma)
dat$CarbonateStage <- as.factor(dat$CarbonateStage)
dat$BioticCrustClass <- as.factor(dat$BioticCrustClass)

dat$DryHue <- as.factor(dat$DryHue)
dat$MoistHue <- as.factor(dat$MoistHue)

# Create Functions for extracting to single value per unique id(plot)
f1 <- function(x) max(x$ClayPercent, na.rm = T) 
f111 <- function(x) min(x$ClayPercent, na.rm = T) 
f2 <- function(x) summary(x[,-1])
f3 <- function(x) max(x$bottom, na.rm = T)
f4 <- function(x) max(x$SandPercent, na.rm = T)
f41 <- function(x) min(x$SandPercent, na.rm = T)
f5 <- function(x) max(x$Elevation, na.rm = T)
f6 <- function(x) max(x$pH, na.rm = T)
f7 <- function(x) min(x$pH, na.rm = T)
f8 <- function(x) max(x$DryValue, na.rm = T)
f9 <- function(x) min(x$DryValue, na.rm = T)
f10 <- function(x) max(x$DryChroma, na.rm = T)
f11 <- function(x) min(x$DryChroma, na.rm = T)
f12 <- function(x) max(x$MoistValue, na.rm = T)
f13 <- function(x) min(x$MoistValue, na.rm = T)
f14 <- function(x) max(x$MoistChroma, na.rm = T)
f15 <- function(x) min(x$MoistChroma, na.rm = T)
f16 <- function(x) max(x$CarbonateStage, na.rm = T)
f17 <- function(x) max(x$BioticCrustClass, na.rm = T)
f18 <- function(x) max(x$Slope, na.rm = T)


# pull out max/min for 1 value per unique id(plot)
 #Numeric
maxClay <- ddply(.data = df1, .(id), .fun = f1)
 names(maxClay)[2] <- 'maxClay'
minClay <- ddply(.data = df1, .(id), .fun = f111)
 names(minClay)[2] <- 'minClay'
maxDepth <- ddply(.data = df1, .(id), .fun = f3)
 names(maxDepth)[2] <- 'maxDepth'
maxSand <- ddply(.data = df1, .(id), .fun = f4)
 names(maxSand)[2] <- 'maxSand'
minSand <- ddply(.data = df1, .(id), .fun = f41)
 names(minSand)[2] <- 'minSand'
Elevation <- ddply(.data = df1, .(id), .fun = f5)
 names(Elevation)[2] <- 'Elevation'
maxpH <- ddply(.data = df1, .(id), .fun = f6)
 names(maxpH)[2] <- 'maxpH'
minpH <- ddply(.data = df1, .(id), .fun = f7)
 names(minpH)[2] <- 'minpH'
Slope <- ddply(.data = df1, .(id), .fun = f18)
 names(Slope)[2] <- 'Slope'

 #Factor
maxDryValue <- ddply(.data = df1, .(id), .fun = f8)
 names(maxDryValue)[2] <- 'maxDryValue'
minDryValue <- ddply(.data = df1, .(id), .fun = f9)
 names(minDryValue)[2] <- 'minDryValue'
maxDryChroma <- ddply(.data = df1, .(id), .fun = f10)
 names(maxDryChroma)[2] <- 'maxDryChroma'
minDryChroma <- ddply(.data = df1, .(id), .fun = f11)
 names(minDryChroma)[2] <- 'minDryChroma'
maxMoistValue <- ddply(.data = df1, .(id), .fun = f12)
 names(maxMoistValue)[2] <- 'maxMoistValue'
minMoistValue <- ddply(.data = df1, .(id), .fun = f13)
 names(minMoistValue)[2] <- 'minMoistValue'
maxMoistChroma <- ddply(.data = df1, .(id), .fun = f14)
 names(maxMoistChroma)[2] <- 'maxMoistChroma'
minMoistChroma <- ddply(.data = df1, .(id), .fun = f15)
 names(minMoistChroma)[2] <- 'minMoistChroma'
CarbonateStage <- ddply(.data = df1, .(id), .fun = f16)
 names(CarbonateStage)[2] <- 'CarbonateStage'
BioticCrustClass <- ddply(.data = df1, .(id), .fun = f17)
 names(BioticCrustClass)[2] <- 'BioticCrustClass'


# Change moist Chroma to 1st horizon and 2nd horizon as a proxy
# for organic carbon accumulation in surface and subsurface horzons
# A lower chroma means more organic carbon. Grassland soils have
# higher fine root turnover.

# Surface
df1$Surface <- as.numeric(df1$top<1) # Turns surface horizon into 1, and all other into 0

max.func.sur <- function(df1) {
  max.moist.sur <- max(df1$Surface)
  
  return(data.frame(Surface = df1$MoistChroma[df1$Surface==max.moist.sur]))
}

Surface <- ddply(df1, .(id), max.func.sur)
names(Surface)[2] <- 'Surface'

# Subsurface
df1 <- getanID(data = df1, id.vars = "id") # Creates an ordered list of each horizon in a plot
df1$Subsurface <- as.numeric(df1$.id==2) # Makes subsurface horizon 1, and all other horizons 0
df1$MoistChroma[is.na(df1$MoistChroma)] <- 0

max.func.sub <- function(df1) {
  max.moist.sub <- max(df1$Subsurface)
  
  return(data.frame(Subsurface = df1$MoistChroma[df1$Subsurface==max.moist.sub]))
}

Subsurface <- ddply(df1, .(id), max.func.sub)
names(Subsurface)[2] <- 'Subsurface'


all <- join(Elevation, SlopeShape, by = 'id', type = 'inner')
all <- join(all, maxClay, by = 'id', type = 'inner')
all <- join(all, minClay, by = 'id', type = 'inner')
all <- join(all, maxSand, by = 'id', type = 'inner')
all <- join(all, minSand, by = 'id', type = 'inner')
all <- join(all, maxDepth, by = 'id', type = 'inner')
all <- join(all, maxpH, by = 'id', type = 'inner')
all <- join(all, minpH, by = 'id', type = 'inner')
all <- join(all, maxDryValue, by = 'id', type = 'inner')
all <- join(all, minDryValue, by = 'id', type = 'inner')
all <- join(all, maxDryChroma, by = 'id', type = 'inner')
all <- join(all, minDryChroma, by = 'id', type = 'inner')
all <- join(all, maxMoistValue, by = 'id', type = 'inner')
all <- join(all, minMoistValue, by = 'id', type = 'inner')
all <- join(all, maxMoistChroma, by = 'id', type = 'inner')
all <- join(all, minMoistChroma, by = 'id', type = 'inner')
all <- join(all, CarbonateStage, by = 'id', type = 'inner')
all <- join(all, BioticCrustClass, by = 'id', type = 'inner')
all <- join(all, Surface, by = 'id', type = 'inner')
all <- join(all, Subsurface, by = 'id', type = 'inner')

# Create new soil parameter where depth is binary.
 # if the maximum depth is >50/100/150/200 then 1, if not then 0
all$Depth50 <- as.numeric(all$maxDepth > 50)
all$Depth100 <- as.numeric(all$maxDepth > 100)
all$Depth150 <- as.numeric(all$maxDepth > 150)
all$Depth200 <- as.numeric(all$maxDepth == 200)



write.csv(all,file="F:/Soils/SoilEnvironmentalData.csv")

# After all this I manually added Colby's AWS data to this file
#   and called it SoilEnvironmentalDataModWithColbyAWS.csv
#   Also remove first column of 12345678....
