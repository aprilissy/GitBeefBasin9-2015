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
dat$depth <- dat$bottom-dat$top
dat <- subset(dat, select = -c(top,bottom,Horizon,Theta_fc,Theta_pwp, HzNum,Texture,SandSize) )

dat$AWCcm <- dat$AWHC*dat$depth

# unique(c(as.character(H2$.id)))

  # Scale Hue - Redness Scale - Degree of Redness
  # I have 4 Hue values:2.5YR, 5YR, 7.5YR, and 10YR
  # They will be numbered from least to most red. 2.5YR=4, 5YR=3, 7.5YR=2, 10YR=1.
{
dat$DryHue <- sub("2.5YR", "4", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- sub("7.5YR", "2", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- sub("5YR", "3", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- sub("10YR", "1", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- as.factor(dat$DryHue)

dat$MoistHue <- sub("2.5YR", "4", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- sub("7.5YR", "2", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- sub("5YR", "3", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- sub("10YR", "1", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- as.factor(dat$MoistHue)

dat$Effervescence <- sub("VE", "4", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("ST", "3", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("SL", "2", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("VS", "1", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("NE", "0", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("LS", "2", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- as.factor(dat$Effervescence)

}

H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
# Pull out data that is not only for horizon 1, then take it out of H1
Plot <- subset(H1, select = c(id,Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))
H1 <- subset(H1, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))


{Plot$SlopeShape <- sub("LC", "CL", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- sub("VC", "CV", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- sub("VL", "LV", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- as.factor(Plot$SlopeShape)}
dat <- subset(dat, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass) )


H2 <- dat[! which(dat$.id=='1'), ] # Pull out horizon #1


# Functions for Max and Min
H2$TotalAWC <- ddply(H2, 'id', summarize, TotalAWC = sum(AWCcm, na.rm = T))

MaxAWC <- ddply(H2, 'id', summarize, MaxAWC = max(AWHC, na.rm = T))
Slope <- ddply( H2, .(id), function(x) max(x$Slope, na.rm = T) )
names(Slope)[2] <- 'Slope'
maxDepth <- ddply( H2, .(id), function(x) max(x$bottom, na.rm = T) )
names(maxDepth)[2] <- 'maxDepth'
maxSand <- ddply( H2, .(id), function(x) max(x$SandPercent, na.rm = T) )
names(maxSand)[2] <- 'maxSand'
minSand <- ddply( H2, .(id), function(x) min(x$SandPercent, na.rm = T) )
names(minSand)[2] <- 'minSand'
minClay <- ddply( H2, .(id), function(x) min(x$ClayPercent, na.rm = T) )
names(minClay)[2] <- 'minClay'
maxClay <- ddply( H2, .(id), function(x) max(x$ClayPercent, na.rm = T) )
names(maxClay)[2] <- 'maxClay'
Elevation <- ddply( H2, .(id), function(x) max(x$Elevation, na.rm = T) )
names(Elevation)[2] <- 'Elevation'
maxpH <- ddply( H2, .(id), function(x) max(x$pH, na.rm = T) )
names(maxpH)[2] <- 'maxpH'
minpH <- ddply( H2, .(id), function(x) min(x$pH, na.rm = T) )
names(minpH)[2] <- 'minpH'
CarbonateStage <- ddply( H2, .(id), function(x) max(x$CarbonateStage, na.rm = T) )
names(CarbonateStage)[2] <- 'CarbonateStage'
BioticCrustClass <- ddply( H2, .(id), function(x) max(x$BioticCrustClass, na.rm = T) )
names(BioticCrustClass)[2] <- 'BioticCrustClass'


#Factor
maxDryValue <- ddply(.data = H2, .(id), function(x) max(x$DryValue, na.rm = T))
names(maxDryValue)[2] <- 'maxDryValue'
minDryValue <- ddply(.data = H2, .(id), function(x) min(x$DryValue, na.rm = T))
names(minDryValue)[2] <- 'minDryValue'
maxDryChroma <- ddply(.data = dat, .(id), function(x) max(x$DryChroma, na.rm = T))
names(maxDryChroma)[2] <- 'maxDryChroma'
minDryChroma <- ddply(.data = dat, .(id), function(x) min(x$DryChroma, na.rm = T))
names(minDryChroma)[2] <- 'minDryChroma'
maxMoistValue <- ddply(.data = dat, .(id), function(x) max(x$MoistValue, na.rm = T))
names(maxMoistValue)[2] <- 'maxMoistValue'
minMoistValue <- ddply(.data = dat, .(id), function(x) min(x$MoistValue, na.rm = T))
names(minMoistValue)[2] <- 'minMoistValue'
maxMoistChroma <- ddply(.data = dat, .(id), function(x) max(x$MoistChroma, na.rm = T))
names(maxMoistChroma)[2] <- 'maxMoistChroma'
minMoistChroma <- ddply(.data = dat, .(id), function(x) min(x$MoistChroma, na.rm = T))
names(minMoistChroma)[2] <- 'minMoistChroma'






colnames(H1) = paste("H1_", colnames(H1)) # Rename variables for H1
rename(H1, c("H1_ id"="id")) 
colnames(H2) = paste("H2_", colnames(H2)) # Rename variables for H1
rename(H2, c("H2_ id"="id")) 
