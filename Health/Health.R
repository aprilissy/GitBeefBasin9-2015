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

la<-read.csv("F:/Health/LeafAreaEpidermalConductance.csv",header=TRUE)
la <- la[ which(!la$Wet.Dry<0.000000), ] # Remove negative weights.
la <- la[,c(1,14:15)]

NP<-read.csv("F:/Health/April Sagebrush N and Protein.csv",header=TRUE)
NP <- NP[,c(2,5:6)]


pdw <- ddply( la, 'Plot', summarize, pdw = mean(PctDryWeight, na.rm = T))
sla <- ddply( la, 'Plot', summarize, sla = mean(SLAcm, na.rm = T))
LA <- join(pdw, sla, by = 'Plot', type = 'inner')


both <- merge(NP, LA, by=c("Plot")) 


hist(LA$pdw)
hist(LA$sla)
hist(NP$NitrogePct)
hist(NP$ProteinPct)
 
# LA <-LA[-c(14),]
# hist(LA$sla)

qqPlot(LA$pdw, main="QQ Plot")
qqPlot(LA$sla, main="QQ Plot")
qqPlot(NP$NitrogePct, main="QQ Plot")
qqPlot(NP$ProteinPct, main="QQ Plot")




Den <- read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE, row.names=1)
Soil <- read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)
Soil <- cbind(Plot = rownames(Soil), Soil)

ARTR2 <- Den$ARTR2
artr <- cbind(Soil,ARTR2) ; artr <- artr[, !sapply(artr, is.factor)] # Combine ARTR with Soils, remove the factor variables
ALA <- merge(LA, Soil, by=c("Plot")) 
ANP <- merge(NP, Soil, by=c("Plot")) 


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

