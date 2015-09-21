 #USGS Soil data
#install.packages('XLConnect')
library(plyr)
library(data.table)
library(splitstackshape)
library(aqp)
library(reshape2)

# Read in Soils data
udat <- read.csv("F:/Soils/USGSsoildataModForAprilsdata.csv", header = T,nrows = 444)
udat <- subset(udat, select = -c(Horizon,Theta_fc,Theta_pwp) )


#Look at data
names(udat)
sapply(udat, class)

# f2 <- function(x) summary(x[,-1])
# summary <- ddply( udat, .(id), function(x) summary(x[,-1]) )

#AWC
TotalAWC <- ddply(udat, 'id', summarize, TotalAWC = sum(AWHC, na.rm = T))
MaxAWC <- ddply(udat, 'id', summarize, MaxAWC = max(AWHC, na.rm = T))
maxDepth <- ddply( udat, .(id), function(x) max(x$bottom, na.rm = T) )
names(maxDepth)[2] <- 'maxDepth'
maxSand <- ddply( udat, .(id), function(x) max(x$SandPercent, na.rm = T) )
names(maxSand)[2] <- 'maxSand'
minSand <- ddply( udat, .(id), function(x) min(x$SandPercent, na.rm = T) )
names(minSand)[2] <- 'minSand'
minClay <- ddply( udat, .(id), function(x) min(x$ClayPercent, na.rm = T) )
names(minClay)[2] <- 'minClay'
maxClay <- ddply( udat, .(id), function(x) max(x$ClayPercent, na.rm = T) )
names(maxClay)[2] <- 'maxClay'
maxpH <- ddply( udat, .(id), function(x) max(x$pH, na.rm = T) )
names(maxpH)[2] <- 'maxpH'
minpH <- ddply( udat, .(id), function(x) min(x$pH, na.rm = T) )
names(minpH)[2] <- 'minpH'


#Factor
maxDryValue <- ddply(.data = udat, .(id), function(x) max(x$DryValue, na.rm = T))
names(maxDryValue)[2] <- 'maxDryValue'
minDryValue <- ddply(.data = udat, .(id), function(x) min(x$DryValue, na.rm = T))
names(minDryValue)[2] <- 'minDryValue'
maxDryChroma <- ddply(.data = udat, .(id), function(x) max(x$DryChroma, na.rm = T))
names(maxDryChroma)[2] <- 'maxDryChroma'
minDryChroma <- ddply(.data = udat, .(id), function(x) min(x$DryChroma, na.rm = T))
names(minDryChroma)[2] <- 'minDryChroma'
maxMoistValue <- ddply(.data = udat, .(id), function(x) max(x$MoistValue, na.rm = T))
names(maxMoistValue)[2] <- 'maxMoistValue'
minMoistValue <- ddply(.data = udat, .(id), function(x) min(x$MoistValue, na.rm = T))
names(minMoistValue)[2] <- 'minMoistValue'
maxMoistChroma <- ddply(.data = udat, .(id), function(x) max(x$MoistChroma, na.rm = T))
names(maxMoistChroma)[2] <- 'maxMoistChroma'
minMoistChroma <- ddply(.data = udat, .(id), function(x) min(x$MoistChroma, na.rm = T))
names(minMoistChroma)[2] <- 'minMoistChroma'

# Change moist Chroma to 1st horizon and 2nd horizon as a proxy
# for organic carbon accumulation in surface and subsurface horzons
# A lower chroma means more organic carbon. Grassland soils have
# higher fine root turnover.

# Surface
udat$Surface <- as.numeric(udat$top<1) # Turns surface horizon into 1, and all other into 0

max.func.sur <- function(udat) {
  max.moist.sur <- max(udat$Surface)
  
  return(data.frame(Surface = udat$MoistChroma[udat$Surface==max.moist.sur]))
}

Surface <- ddply(udat, .(id), max.func.sur)
names(Surface)[2] <- 'Surface'

# Subsurface
udat <- getanID(data = udat, id.vars = "id") # Creates an ordered list of each horizon in a plot
udat$Subsurface <- as.numeric(udat$.id==2) # Makes subsurface horizon 1, and all other horizons 0
udat$MoistChroma[is.na(udat$MoistChroma)] <- 0

max.func.sub <- function(udat) {
  max.moist.sub <- max(udat$Subsurface)
  
  return(data.frame(Subsurface = udat$MoistChroma[udat$Subsurface==max.moist.sub]))
}

Subsurface <- ddply(udat, .(id), max.func.sub)
names(Subsurface)[2] <- 'Subsurface'

#Numeric data
udat$top <- as.numeric(udat$top)
udat$bottom <- as.numeric(udat$bottom)
udat$ClayPercent <- as.numeric(udat$ClayPercent)
udat$SandPercent <- as.numeric(udat$SandPercent)
udat$pH <- as.numeric(udat$pH)
udat$AWHC <- as.numeric(udat$AWHC)

# Factor data
udat$DryHue <- as.factor(udat$DryHue)
udat$DryValue <- as.factor(udat$DryValue)
udat$DryChroma <- as.factor(udat$DryChroma)
udat$MoistHue <- as.factor(udat$MoistHue)
udat$MoistValue <- as.factor(udat$MoistValue)
udat$MoistChroma <- as.factor(udat$MoistChroma)
udat$Texture <- as.factor(udat$Texture)
udat$SandSize <- as.factor(udat$SandSize)
udat$Effervescence <- as.factor(udat$Effervescence)
udat$HzNum <- as.factor(udat$HzNum)


all <- join(maxClay, minClay, by = 'id', type = 'inner')
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
all <- join(all, Surface, by = 'id', type = 'inner')
all <- join(all, Subsurface, by = 'id', type = 'inner')


# Create new soil parameter where depth is binary.
# if the maximum depth is >50/100/150/200 then 1, if not then 0
all$Depth50 <- as.numeric(all$maxDepth > 50)
all$Depth100 <- as.numeric(all$maxDepth > 100)
all$Depth150 <- as.numeric(all$maxDepth > 150)
all$Depth200 <- as.numeric(all$maxDepth == 200)

#Now calculate depth weighted averages of each continuous variable, then append these to the other variables. 
#Convert to SoilProfileCollection
depths(udat) <- id ~ top + bottom

# within each profile, compute weighted means, over the intervals: 0-25,0-50,0-100, removing NA if present 
d25 <- slab(udat, id ~ AWHC, slab.structure = c(0,25), slab.fun = mean, na.rm=TRUE)
d50 <- slab(udat, id ~ AWHC, slab.structure = c(0,50), slab.fun = mean, na.rm=TRUE)
d100 <- slab(udat, id ~ AWHC, slab.structure = c(0,100), slab.fun = mean, na.rm=TRUE)

# reshape to wide format, remove unneeded variables and rename. 
AWC25 <- dcast(d25, id + top + bottom ~ variable, value.var = 'value')
AWC25 <- AWC25[,-c(2,3)]
names(AWC25)[2] <- 'AWC25'

AWC50 <- dcast(d50, id + top + bottom ~ variable, value.var = 'value')
AWC50 <- AWC50[,-c(2,3)]
names(AWC50)[2] <- 'AWC50'

AWC100 <- dcast(d100, id + top + bottom ~ variable, value.var = 'value')
AWC100 <- AWC100[,-c(2,3)]
names(AWC100)[2] <- 'AWC100'

all <- join(all, AWC25, by = 'id', type = 'inner')
all <- join(all, AWC50, by = 'id', type = 'inner')
all <- join(all, AWC100, by = 'id', type = 'inner')
all <- join(all, MaxAWC, by = 'id', type = 'inner')
all <- join(all, TotalAWC, by = 'id', type = 'inner')

write.csv(all,file="F:/Soils/SoilEnvironmentaldataUSGS.csv", row.names=FALSE)

# Add in Elevation, Slope Shape, Slope, Carbonate Stage, Biotic Crust Class
data <- read.csv("F:/Soils/SoilEnvironmentaldataUSGS.csv")
site <- read.csv("F:/BeefBasin Data For April/BeefBasin/formattedR/Site_Data.csv")
loc <- read.csv("F:/BeefBasin Data For April/BeefBasin/formattedR/locInfo.csv")

# Remove BLM Trend and Miller plots
site <- site[-c(66:77),]
site <- site[order(site$pedonID),] # Sort so plot 100 is by 9 same as data
loc <- loc[-c(66:77),]
loc <- loc[order(loc$Plot.Name),] # Sort so plot 100 is by 9 same as data

data$Elevation <- loc$altitude
data$Slope <- site$Slope
data$SlopeShape <- site$SlopeShape
data$CarbonateStage <- site$CarbonateStage
data$BioticCrustClass <- site$BioticCrustClass

# Combine Slope Shape categories
data$SlopeShape <- sub("LC", "CL", data$SlopeShape, ignore.case = FALSE)
data$SlopeShape <- sub("VC", "CV", data$SlopeShape, ignore.case = FALSE)
data$SlopeShape <- sub("VL", "LV", data$SlopeShape, ignore.case = FALSE)
data$SlopeShape <- as.factor(data$SlopeShape)

# replace -inf with na in min and max pH
is.na(data$minpH) <- !is.finite(data$minpH) 
is.na(data$maxpH) <- !is.finite(data$maxpH) 

write.csv(data,file="F:/Soils/SoilEnvironmentaldataUSGS.csv", row.names=FALSE)

#####
# Add to April Soils

april  <- read.csv("F:/Soils/SoilEnvironmentaldata.csv")

total <- rbind(april, data)
