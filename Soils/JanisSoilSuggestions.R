# Soils Data Based on Janis Boettinger's Suggestions

# 1. Surface horizon for all variables
# 2. All other horizons together
# 3. Bring Colby's Data points that are in my study area into my 99
# 4. Scale Hue - Redness Scale or Degree of Redness 10YR=1, 7.5YR=2, 2.5YR=3, etc...
# 5. Value May be the most informative, high value means carbonates, medium = low organic matter, low = high organic matter
# 6. Chroma Classes: low=1&2, medium=3&4, high=6&8. Can number these 1,2,3 and use as continuous not categorical for NMDS
# 7. AWC depth weighted average. AWC*horizon depth gives you H2O in cm for a horizon.
# 8. Use this to find how much water is in the top 50cm(example)
# 9. Gini index looks at node impurity, how pure are the nodes, how well is it able to split into categories?

library(splitstackshape)
library(plyr)


# Read in data
dat <- read.csv("F:/Soils/SoilDataFitUSGSColumns.csv", header = T,nrows = 444)
dat <- getanID(data = dat, id.vars = "id") # Creates an ordered list of each horizon in a plot

H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
H1 <- subset(H1, select = -c(Horizon,top,.id, HzNum) ) # Remove top, Horizon, HzNum, .id
# Pull out data that is not only for horizon 1, then take it out of H1
Plot <- subset(H1, select = c(id,Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))
H1 <- subset(H1, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))
colnames(H1) = paste("H1_", colnames(H1)) # Rename variables for H1
rename(H1, c("H1_ id"="id", "H1_ bottom"="H1_ Depth")) 






# Surface
dat$Surface <- as.numeric(dat$top<1) # Turns surface horizon into 1, and all other into 0

max.func.sur <- function(dat) {
  max.moist.sur <- max(dat$Surface)
  
  return(data.frame(Surface = dat$MoistChroma[dat$Surface==max.moist.sur]))
}

Surface <- ddply(dat, .(id), max.func.sur)
names(Surface)[2] <- 'Surface'

# Subsurface
dat$Subsurface <- as.numeric(dat$.id==2) # Makes subsurface horizon 1, and all other horizons 0
dat$MoistChroma[is.na(dat$MoistChroma)] <- 0

max.func.sub <- function(dat) {
  max.moist.sub <- max(dat$Subsurface)
  
  return(data.frame(Subsurface = dat$MoistChroma[dat$Subsurface==max.moist.sub]))
}

Subsurface <- ddply(dat, .(id), max.func.sub)
names(Subsurface)[2] <- 'Subsurface'
