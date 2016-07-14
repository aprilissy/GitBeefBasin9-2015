# Health

library(plyr)

### % Dry Weight ### (PDW)
# Dry/Fresh
# if plant is better hydrated the # will be low
# if plant is dessicated # will be higher


### Specific Leaf Area ### (SLA) 
# m2/g(dry) 
# thicker leaves less efficient but tougher (lower SLA)
# look at other peoples SLA
# typical (Bruce Bugbee) is 100 to 600

LA<-read.csv("F:/Health/LeafAreaEpidermalConductance.csv",header=TRUE)
LA <- LA[ which(!LA$Wet.Dry<0.000000), ] # Remove negative weights.
LA <- LA[,c(1,14:15)]

NP<-read.csv("F:/Health/April Sagebrush N and Protein.csv",header=TRUE)
NP <- NP[,c(2,5:6)]

# find means from LA
pdw <- ddply( LA, 'Plot', summarize, pdw = mean(PctDryWeight, na.rm = T))
sla <- ddply( LA, 'Plot', summarize, sla = mean(SLAcm, na.rm = T))
LA <- join(pdw, sla, by = 'Plot', type = 'inner')

# What plots do LA and NP have in common?
LA.NP <- merge(NP, LA, by=c("Plot")) 

# Look for data normality. Not found in these 4 variables
hist(LA$pdw)
hist(LA$sla)
hist(NP$NitrogePct)
hist(NP$ProteinPct)
 
qqnorm(LA$pdw); qqline(LA$pdw)
qqnorm(LA$sla); qqline(LA$sla)
qqnorm(NP$NitrogePct); qqline(NP$NitrogePct)
qqnorm(NP$ProteinPct); qqline(NP$ProteinPct)


# Read in Density and Soils Data
Den <- read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE)
Den <- rename(Den, c("X"="Plot"))
Soil <- read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE)
Soil <- rename(Soil, c("id"="Plot"))
Height1 <- read.csv("F:/ShrubDensity/HeightClass/Output/AprilLiveDeadPlotbySizeClass.csv",header=TRUE,row.names=1)
Height2 <- read.csv("F:/ShrubDensity/HeightClass/Output/AprilSizeClassLiveDeadDensityM2.csv",header=TRUE,row.names=1)


# # Simple Bar Plot 
# counts <- table(Height1$E)
# barplot(counts, main=">100cm Sagebrush", 
#         xlab="Number of Sagebrush")
# 
# Height3<-data.matrix(Height1)
# # Stacked Bar Plot with Colors and Legend
# sums <- colSums (Height1, na.rm = FALSE, dims = 1)
# counts <- table(Height3[,1],Height3[,2],Height3[,3],Height3[,4],Height3[,5])
# barplot(sums, main="Car Distribution by Gears and VS",
#         xlab="Number of Gears", col=c("darkblue","red"),
#         legend = rownames(counts))
# 
# # Grouped Bar Plot
# colours <- c("red", "orange", "blue", "yellow", "green")
# bplt <- barplot(as.matrix(sums), main="Sagebrush Counts by Height Class",xlab="Height Class", ylab = "Counts", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours,names.arg = c("A", "B", "C","D","E"))
# text(x= counts+0.3, y= bplt, labels=as.character(counts), xpd=TRUE)
# 
# counts <- table(Height3[,1],Height3[,2],Height3[,3],Height3[,4],Height3[,5])
# barplot(counts, main="Car Distribution by Gears and VS",
#         xlab="Number of Gears", col=c("darkblue","red"),
#         legend = rownames(counts), beside=TRUE)

# Pull out ARTR2 and combine with soils, LA, NP
ARTR2 <- Den$ARTR2
artr <- cbind(Soil,ARTR2); rownames(artr) <- artr[,1]
artr <- artr[, !sapply(artr, is.factor)] # Combine ARTR with Soils, remove the factor variables
artr$Plot<-rownames(artr)

ALA <- merge(LA, Soil, by=c("Plot")) 
ANP <- merge(NP, Soil, by=c("Plot")) 


# Look at LA or NP with ARTR2
art <- Den[,c(1,5)]
LAhealth <- merge(LA, art, by=c("Plot")) 
NPhealth <- merge(NP, art, by=c("Plot")) 

# Look at LA or NP with ARTR2 and soils
ALAhealth <- merge(ALA, art, by=c("Plot")) 
ANPhealth <- merge(ANP, art, by=c("Plot")) 


library(randomForest)
# Look at LA and NP in random forests
  #Without Soils

live.rf = randomForest(as.numeric(ARTR2) ~ pdw+sla
                       , data = LAhealth,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,mtry=2,
                       ntree = 500)
#var explained printed
print(live.rf)
varImpPlot(live.rf)

  #With Soils
live.rf = randomForest(as.numeric(ARTR2) ~ pdw+sla+Elevation+DepthClass
                       , data = ALAhealth,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,mtry=2,
                       ntree = 500)

#var explained printed
print(live.rf)
varImpPlot(live.rf)

live.rf = randomForest(as.numeric(ARTR2) ~ NitrogePct+ProteinPct
                       , data = ANPhealth,proximity=TRUE,
                       importance=TRUE,keep.forest=TRUE,
                       na.action = na.omit,mtry=2,
                       ntree = 500)

#var explained printed
print(live.rf)



# Look at correlations between Health and Soil variables

# panel.smooth function is built in.
# panel.cor puts correlation in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# Correlations with significance levels
library(Hmisc)
rcorr((data.matrix(LAhealth,rownames.force=T)), type="pearson") # type can be pearson or spearman
plot(LAhealth$pdw,LAhealth$ARTR2)
abline(lm(LAhealth$pdw~LAhealth$ARTR2), col="red") # regression line (y~x) 
lines(lowess(LAhealth$pdw,LAhealth$ARTR2), col="blue") # lowess line (x,y)

p<-a$P
r<-a$r
a<-rcorr((data.matrix(ALAhealth,rownames.force=T)), type="pearson") # type can be pearson or spearman

df <- data.frame(matrix(unlist(a), nrow=52, byrow=T),stringsAsFactors=FALSE)

# ARTR2
pairs(~pdw+sla+ARTR2,data=LAhealth, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="LA Variables")
# ARTR2
pairs(~NitrogePct+ProteinPct+ARTR2,data=NPhealth, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="NP Variables")

plot(LAhealth)
plot(NPhealth)

# Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay Variables")

# Sand
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Sand Variables")


# pH
pairs(~MaxpH+DWApH+H1.pH+pH.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="pH Variables")


# AWHC
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="AWHC Variables")


# Effervescence
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Effervescence Variables")


# DryValue
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="DryValue Variables")


art <- cbind(Soil,ARTR2)
# Depth
pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Depth Variables")

# Factors
pairs(~SlopeShape+Tot.Texture+Tot.SandSize+H1.Texture+H1.SandSize+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Factor Variables")


# 
pairs(~H1.SandPercent+DWASand+DWA.AWHC+MaxAWHC+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.SandPercent+H1.ClayPercent+H1.Depth+H1.DWA_AWC+H1.pH+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+Sand.50+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+sla+pdw,data=ALA, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")










# Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay Variables")

# Sand
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Sand Variables")


# pH
pairs(~MaxpH+DWApH+H1.pH+pH.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="pH Variables")


# AWHC
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="AWHC Variables")


# Effervescence
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Effervescence Variables")


# DryValue
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="DryValue Variables")


art <- cbind(Soil,ARTR2)
# Depth
pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Depth Variables")

# Factors
pairs(~SlopeShape+Tot.Texture+Tot.SandSize+H1.Texture+H1.SandSize+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Factor Variables")


# 
pairs(~H1.SandPercent+DWASand+DWA.AWHC+MaxAWHC+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.SandPercent+H1.ClayPercent+H1.Depth+H1.DWA_AWC+H1.pH+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+Sand.50+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+NitrogePct+ProteinPct,data=ANP, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

