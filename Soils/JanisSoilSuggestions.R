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
# Manually in excel add in second IL1_9 row of NA so H2 still has 99obs
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
TotalAWC <- ddply(H2, 'id', summarize, TotalAWC = sum(AWCcm, na.rm = T))
MaxAWC <- ddply(H2, 'id', summarize, MaxAWC = max(AWCcm, na.rm = T))
MaxDepth <- ddply(H2, 'id', summarize, MaxDepth = max(depth, na.rm = T))
MaxClay <- ddply( H2, 'id', summarize, MaxClay = max(ClayPercent, na.rm = T))
MinClay <- ddply( H2, 'id', summarize, MinClay = min(ClayPercent, na.rm = T))
MaxSand <- ddply(H2, 'id', summarize, MaxSand = max(SandPercent, na.rm = T))
MinSand <- ddply(H2, 'id', summarize, MinSand = min(SandPercent, na.rm = T))
MaxpH <- ddply(H2, 'id', summarize, MaxpH = max(pH, na.rm = T))
MinpH <- ddply(H2, 'id', summarize, MinpH = min(pH, na.rm = T))
MaxDryHue <- ddply(H2, 'id', summarize, MaxDryHue = max(DryHue, na.rm = T))
MinDryHue <- ddply(H2, 'id', summarize, MinDryHue = min(DryHue, na.rm = T))
MaxDryValue <- ddply(H2, 'id', summarize, MaxDryValue = max(DryValue, na.rm = T))
MinDryValue <- ddply(H2, 'id', summarize, MinDryValue = min(DryValue, na.rm = T))
MaxDryChroma <- ddply(H2, 'id', summarize, MaxDryChroma = max(DryChroma, na.rm = T))
MinDryChroma <- ddply(H2, 'id', summarize, MinDryChroma = min(DryChroma, na.rm = T))
MaxMoistHue <- ddply(H2, 'id', summarize, MaxMoistHue = max(MoistHue, na.rm = T))
MinMoistHue <- ddply(H2, 'id', summarize, MinMoistHue = min(MoistHue, na.rm = T))
MaxMoistValue <- ddply(H2, 'id', summarize, MaxMoistValue = max(MoistValue, na.rm = T))
MinMoistValue <- ddply(H2, 'id', summarize, MinMoistValue = min(MoistValue, na.rm = T))
MaxMoistChroma <- ddply(H2, 'id', summarize, MaxMoistChroma = max(MoistChroma, na.rm = T))
MinMoistChroma <- ddply(H2, 'id', summarize, MinMoistChroma = min(MoistChroma, na.rm = T))



is.na(dat$MaxClay) <- !is.finite(dat$MaxClay) 
is.na(dat$MinClay) <- !is.finite(dat$MinClay) 
is.na(dat$MaxAWC) <- !is.finite(dat$MaxAWC) 


colnames(H1) = paste("H1_", colnames(H1)) # Rename variables for H1
rename(H1, c("H1_ id"="id")) 
colnames(H2) = paste("H2_", colnames(H2)) # Rename variables for H1
rename(H2, c("H2_ id"="id")) 
