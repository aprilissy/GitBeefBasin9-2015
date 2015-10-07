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


# Scale Hue - Redness Scale - Degree of Redness
# I have 4 Hue values:2.5YR, 5YR, 7.5YR, and 10YR
# They will be numbered from least(1) to most(4) red. 2.5YR=4, 5YR=3, 7.5YR=2, 10YR=1.

dat$Dry2 <- dat$DryHue
dat$Moist2 <- dat$MoistHue
dat$Effer2 <- dat$Effervescence

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
  
  rename(dat, c("DryHue"="DryRed"))
  rename(dat, c("Dry2"="DryHue"))
  rename(dat, c("MoistHue"="MoistRed"))
  rename(dat, c("Moist2"="MoistHue"))
  rename(dat, c("Effervescence"="EfferScale")) 
  rename(dat, c("Effer2"="Effervescence"))
}

# write file before removing anything for use in slabs function below
write.csv(dat,file="F:/Soils/SoilDataAprilUSGSnotremoved.csv", row.names=FALSE)
# # remove anything you don't want duplicated in H1 and H2
# dat <- subset(dat, select = -c(top,bottom,Horizon,Theta_fc,Theta_pwp, HzNum,Texture,SandSize) )

H1 <- dat[ which(dat$.id=='1'), ] # Pull out horizon #1
H1 <- subset(H1, select=-c(.id))

H2 <- dat[! which(dat$.id=='1'), ] # Pull out horizon #1
H2 <- subset(H2, select=-c(.id))



#Now calculate depth weighted averages of each continuous variable, then append these to the other variables. 
#Convert to SoilProfileCollection
data <- read.csv("F:/Soils/SoilDataAprilUSGSnotremoved.csv", header = T)
depths(data) <- id ~ top + bottom

# within each profile, compute weighted means, over the intervals: 0-25,0-50,0-100,0-150,0-200 removing NA if present 
d25 <- slab(data, id ~ AWHC, slab.structure = c(0,25), slab.fun = mean, na.rm=TRUE)
d50 <- slab(data, id ~ AWHC, slab.structure = c(0,50), slab.fun = mean, na.rm=TRUE)
d100 <- slab(data, id ~ AWHC, slab.structure = c(0,100), slab.fun = mean, na.rm=TRUE)
d150 <- slab(data, id ~ AWHC, slab.structure = c(0,150), slab.fun = mean, na.rm=TRUE)
d200 <- slab(data, id ~ AWHC, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)


s50 <- slab(data, id ~ AWHC, slab.structure = c(25,50), slab.fun = mean, na.rm=TRUE)
s100 <- slab(data, id ~ AWHC, slab.structure = c(50,100), slab.fun = mean, na.rm=TRUE)
s150 <- slab(data, id ~ AWHC, slab.structure = c(100,150), slab.fun = mean, na.rm=TRUE)
s200 <- slab(data, id ~ AWHC, slab.structure = c(150,200), slab.fun = mean, na.rm=TRUE)


# reshape to wide format, remove unneeded variables and rename. 
AWC.0.25 <- dcast(d25, id + top + bottom ~ variable, value.var = 'value')
AWC.0.25 <- AWC.0.25[,-c(2,3)]
names(AWC.0.25)[2] <- 'AWC.0.25'

AWC.0.50 <- dcast(d50, id + top + bottom ~ variable, value.var = 'value')
AWC.0.50 <- AWC.0.50[,-c(2,3)]
names(AWC.0.50)[2] <- 'AWC.0.50'

AWC.0.100 <- dcast(d100, id + top + bottom ~ variable, value.var = 'value')
AWC.0.100 <- AWC.0.100[,-c(2,3)]
names(AWC.0.100)[2] <- 'AWC.0.100'

AWC.0.150 <- dcast(d150, id + top + bottom ~ variable, value.var = 'value')
AWC.0.150 <- AWC.0.150[,-c(2,3)]
names(AWC.0.150)[2] <- 'AWC.0.150'

AWC.0.200 <- dcast(d200, id + top + bottom ~ variable, value.var = 'value')
AWC.0.200 <- AWC.0.200[,-c(2,3)]
names(AWC.0.200)[2] <- 'AWC.0.200'

AWC.25.50 <- dcast(s50, id + top + bottom ~ variable, value.var = 'value')
AWC.25.50 <- AWC.25.50[,-c(2,3)]
names(AWC.25.50)[2] <- 'AWC.25.50'

AWC.50.100 <- dcast(s100, id + top + bottom ~ variable, value.var = 'value')
AWC.50.100 <- AWC.50.100[,-c(2,3)]
names(AWC.50.100)[2] <- 'AWC.50.100'

AWC.100.150 <- dcast(s150, id + top + bottom ~ variable, value.var = 'value')
AWC.100.150 <- AWC.100.150[,-c(2,3)]
names(AWC.100.150)[2] <- 'AWC.100.150'

AWC.150.200 <- dcast(s200, id + top + bottom ~ variable, value.var = 'value')
AWC.150.200 <- AWC.150.200[,-c(2,3)]
names(AWC.150.200)[2] <- 'AWC.150.200'


# within each profile, compute weighted means, removing NA if present 
dwaclay <- slab(data, id ~ ClayPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
dwasand <- slab(data, id ~ SandPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
dwapH <- slab(data, id ~ pH, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)

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

#Now calculate depth weighted averages of H2
#Convert to SoilProfileCollection
Sub <- H2
Sub$DryRed <- as.numeric(Sub$DryRed)
Sub$MoistRed <- as.numeric(Sub$MoistRed)
Sub$EfferScale <- as.numeric(Sub$EfferScale)
depths(Sub) <- id ~ top + bottom

# within each profile, compute weighted means, removing NA if present 
subdwaclay <- slab(Sub, id ~ ClayPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
subdwasand <- slab(Sub, id ~ SandPercent, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
subdwapH <- slab(Sub, id ~ pH, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
subawhc <- slab(Sub, id ~ AWHC, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)
subdryred <- slab(Sub, id ~ DryRed, slab.structure = c(0,200), slab.fun = mean, na.rm=TRUE)

# compute slice-wise probability so that it sums to contributing fraction, from 0-150
# a <- slab(Sub, fm= ~ Effervescence, cpm=1, slab.structure=0:200)
# b <- slab(Sub, id ~ Effervescence, cpm=1, slab.structure=0:200)
# 
# c <- slab(Sub, fm= ~ DryHue, cpm=1, slab.structure=0:200)
# d <- slab(Sub, id ~ DryHue, cpm=1, slab.structure=0:200)
# e <- slab(Sub, fm= ~ DryHue, cpm=2, slab.structure=0:200)
# f <- slab(Sub, fm= id ~ DryHue, cpm=1, slab.structure=0:200)
g <- slab(Sub, id ~ DryHue, slab.structure = c(0,200), na.rm=TRUE)


# reshape into long format for plotting
g.long <- melt(g, id.vars=c('id'), measure.vars=c('X10YR','X2.5YR','X5YR','X7.5YR'))
# reshape to wide format, remove unneeded variables and rename. 
gg.long <- dcast(g.long, id ~ variable, value.var = 'value')






# reshape to wide format, remove unneeded variables and rename. 
SubDWAClay <- dcast(subdwaclay, id + top + bottom ~ variable, value.var = 'value')
SubDWAClay <- SubDWAClay[,-c(2,3)]
names(SubDWAClay)[2] <- 'SubDWAClay'

SubDWASand <- dcast(subdwasand, id + top + bottom ~ variable, value.var = 'value')
SubDWASand <- SubDWASand[,-c(2,3)]
names(SubDWASand)[2] <- 'SubDWASand'

SubDWApH <- dcast(subdwapH, id + top + bottom ~ variable, value.var = 'value')
SubDWApH <- SubDWApH[,-c(2,3)]
names(SubDWApH)[2] <- 'SubDWApH'

SubAWC <- dcast(subawhc, id + top + bottom ~ variable, value.var = 'value')
SubAWC <- SubAWC[,-c(2,3)]
names(SubAWC)[2] <- 'SubAWC'




slabs <- join(AWC.0.25, AWC.0.50, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.0.100, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.0.150, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.0.200, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.25.50, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.50.100, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.100.150, by = 'id', type = 'inner')
slabs <- join(slabs, AWC.150.200, by = 'id', type = 'inner')

slabs <- join(slabs, DWAClay, by = 'id', type = 'inner')
slabs <- join(slabs, DWASand, by = 'id', type = 'inner')
slabs <- join(slabs, DWApH, by = 'id', type = 'inner')

slabs <- join(slabs, SubDWAClay, by = 'id', type = 'inner')
slabs <- join(slabs, SubDWASand, by = 'id', type = 'inner')
slabs <- join(slabs, SubDWApH, by = 'id', type = 'inner')
slabs <- join(slabs, SubAWC, by = 'id', type = 'inner')
