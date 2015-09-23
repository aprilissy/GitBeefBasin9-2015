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
library(aqp)
library(data.table)
library(reshape2)
library(tidyr)

# Read in data
dat <- read.csv("F:/Soils/SoilDataFitUSGSColumns.csv", header = T,nrows = 444)
udat <- read.csv("F:/Soils/USGSsoildataModForAprilsdata.csv", header = T,nrows = 444)
udat$id <- extract_numeric(udat$id) # removes CLHS and P leaving only numbers.

# Manually in excel I added in second IL1_9 row of NA so H2 still has 99obs

# Creates an ordered list of each horizon in a plot
dat <- getanID(data = dat, id.vars = "id") 
udat <- getanID(data = udat, id.vars = "id")

# get the depth of each horizon
dat$depth <- dat$bottom-dat$top
udat$depth <- udat$bottom-udat$top

# AWC (cmH2O/cmSoil) * horizon depth(cmSoil)=AWC(cmH2O)
dat$AWCcm <- (dat$AWHC*dat$depth)
udat$AWCcm <- (udat$AWHC*udat$depth)


H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
# Pull out data that is not only for horizon 1, then take it out of H1
Plot <- subset(H1, select = c(id,Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))
H1 <- subset(H1, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass))
dat <- subset(dat, select = -c(Elevation,Aspect,Slope,SlopeShape,CarbonateStage,BioticCrustClass) )

# Combine April and USGS soils data
dat <- rbind(dat,udat)
write.csv(dat,file="F:/Soils/SoilDataAprilUSGSnotremoved.csv", row.names=FALSE)
dat <- subset(dat, select = -c(top,bottom,Horizon,Theta_fc,Theta_pwp, HzNum,Texture,SandSize) )
dat <- subset(dat, select = -c(AWHC,AWCcm) )

# unique(c(as.character(Plot$SlopeShape)))

  # Scale Hue - Redness Scale - Degree of Redness
  # I have 4 Hue values:2.5YR, 5YR, 7.5YR, and 10YR
  # They will be numbered from least to most red. 2.5YR=4, 5YR=3, 7.5YR=2, 10YR=1.
{
dat$DryHue <- sub("2.5YR", "4", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- sub("7.5YR", "2", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- sub("5YR", "3", dat$DryHue, ignore.case = FALSE)
dat$DryHue <- sub("10YR", "1", dat$DryHue, ignore.case = FALSE)

dat$MoistHue <- sub("2.5YR", "4", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- sub("7.5YR", "2", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- sub("5YR", "3", dat$MoistHue, ignore.case = FALSE)
dat$MoistHue <- sub("10YR", "1", dat$MoistHue, ignore.case = FALSE)

dat$Effervescence <- sub("VE", "4", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("ST", "3", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("SL", "2", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("VS", "1", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("NE", "0", dat$Effervescence, ignore.case = FALSE)
dat$Effervescence <- sub("LS", "2", dat$Effervescence, ignore.case = FALSE)
}

H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
H1$Depth <- H1$depth
H1 <- subset(H1, select=-c(.id,depth))

# Add in Elevation, Slope Shape, Slope, Carbonate Stage, Biotic Crust Class
site <- read.csv("F:/BeefBasin Data For April/BeefBasin/formattedR/Site_Data.csv")
loc <- read.csv("F:/BeefBasin Data For April/BeefBasin/formattedR/locInfo.csv")

# Remove BLM Trend and Miller plots
site <- site[-c(66:77),]
site <- site[order(site$pedonID),] # Sort so plot 100 is by 9 same as data
site$pedonID <- extract_numeric(site$pedonID) # removes CLHS and P leaving only numbers.
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

{Plot$SlopeShape <- sub("LC", "CL", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- sub("VC", "CV", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- sub("VL", "LV", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- sub("LVQ", "LV", Plot$SlopeShape, ignore.case = FALSE)
Plot$SlopeShape <- as.factor(Plot$SlopeShape)}

H2 <- dat[! which(dat$.id=='1'), ] # Pull out horizon #1



# Functions for Max and Min
TotalAWC <- ddply(H2, 'id', summarize, TotalAWC = sum(AWCcm, na.rm = T))
MaxAWC <- ddply(H2, 'id', summarize, MaxAWC = max(AWCcm, na.rm = T))
TotalDepth <- ddply(H2, 'id', summarize, TotalDepth = max(depth, na.rm = T))
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


all <- join(MaxClay, MinClay, by = 'id', type = 'inner')
all <- join(all, MaxSand, by = 'id', type = 'inner')
all <- join(all, MinSand, by = 'id', type = 'inner')
all <- join(all, MaxpH, by = 'id', type = 'inner')
all <- join(all, MinpH, by = 'id', type = 'inner')
all <- join(all, MaxDryHue, by = 'id', type = 'inner')
all <- join(all, MinDryHue, by = 'id', type = 'inner')
all <- join(all, MaxDryValue, by = 'id', type = 'inner')
all <- join(all, MinDryValue, by = 'id', type = 'inner')
all <- join(all, MaxDryChroma, by = 'id', type = 'inner')
all <- join(all, MinDryChroma, by = 'id', type = 'inner')
all <- join(all, MaxMoistHue, by = 'id', type = 'inner')
all <- join(all, MinMoistHue, by = 'id', type = 'inner')
all <- join(all, MaxMoistValue, by = 'id', type = 'inner')
all <- join(all, MinMoistValue, by = 'id', type = 'inner')
all <- join(all, MaxMoistChroma, by = 'id', type = 'inner')
all <- join(all, MinMoistChroma, by = 'id', type = 'inner')
all <- join(all, TotalDepth, by = 'id', type = 'inner')


#Now calculate depth weighted averages of each continuous variable, then append these to the other variables. 
#Convert to SoilProfileCollection
data <- read.csv("F:/Soils/SoilDataAprilUSGSnotremoved.csv", header = T)
depths(data) <- id ~ top + bottom

# within each profile, compute weighted means, over the intervals: 0-25,0-50,0-100, removing NA if present 
d25 <- slab(data, id ~ AWHC, slab.structure = c(0,25), slab.fun = mean, na.rm=TRUE)
d50 <- slab(data, id ~ AWHC, slab.structure = c(0,50), slab.fun = mean, na.rm=TRUE)
d100 <- slab(data, id ~ AWHC, slab.structure = c(0,100), slab.fun = mean, na.rm=TRUE)

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

# all <- join(all, MaxAWC, by = 'id', type = 'inner')
# all <- join(all, TotalAWC, by = 'id', type = 'inner')
all <- join(all, AWC25, by = 'id', type = 'inner')
all <- join(all, AWC50, by = 'id', type = 'inner')
all <- join(all, AWC100, by = 'id', type = 'inner')

is.na(all) <- sapply(all, is.infinite) # replace inf- with NA

colnames(H1) = paste("H1", sep="_", colnames(H1)) # Rename variables for H1
rename(H1, c("H1_id"="id")) 
colnames(all) = paste("H2",sep="_", colnames(all)) # Rename variables for H1
names(all)[names(all)=="H2_id"]<-"id"
names(all)[names(all)=="H2_TotalDepth"]<-"TotalDepth"
names(all)[names(all)=="H2_AWC25"]<-"AWC25"
names(all)[names(all)=="H2_AWC50"]<-"AWC50"
names(all)[names(all)=="H2_AWC100"]<-"AWC100"

# Create new soil parameter where depth is binary.
# if the maximum depth is >50/100/150/200 then 1, if not then 0
all$Depth50 <- as.numeric('all$H2_ MaxDepth' > 50)
all$Depth100 <- as.numeric('all$H2_ MaxDepth' > 100)
all$Depth150 <- as.numeric('all$H2_ MaxDepth' > 150)
all$Depth200 <- as.numeric('all$H2_ MaxDepth' == 200)

Soils <- merge(H1,all,by='id')
Soils <- merge(Soils,Plot,by='id')

#####
# Keep only Soils data that has matching veg data.
# Add to April Soils
SoilstoKeep <- Soils[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),]
April1 <- Soils[c(1:99),]
Veg <- rbind(April1, SoilstoKeep)

# Add USGS points in N and S plain to April
USGSinNSplain <- SoilstoKeep[c("19","24","33","39","43","44","47","48","50"),]
NSveg <- rbind(April1,USGSinNSplain)

# USGS & April
write.csv(Veg,file="F:/Soils/SoilEnvironmentaldataUSGSApril.csv", row.names=FALSE)
# USGS in N&S plain & April
write.csv(NSveg,file="F:/Soils/SoilEnvironmentaldataNSplain.csv", row.names=FALSE)
# April
write.csv(April1,file="F:/Soils/SoilEnvironmentaldataApril.csv", row.names=FALSE)
