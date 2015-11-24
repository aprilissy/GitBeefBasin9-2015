# Soil Variables Subset based on correlation and VIF

soil<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
soila<- soil[c(1:99),]
rownames(soil)[rowSums(is.na(soil)) > 0]

# Clay, Sand, & pH = DWA & H1
# Effervescence = Max & H1
# Color = Moist?
# Sand over AWHC



Sub <- subset(soil, select = c(DWAClay,DWASand,DWApH,H1.DWA_AWC,
                                  H1.ClayPercent,H1.SandPercent,H1.Texture,
                                  H1.SandSize,H1.pH,H1.EfferScale,H1.MoistRed,
                                  H1.MoistCClass,H1.MoistValue,Tot.MoistRed,
                                  Tot.MoistCClass,Tot.MoistValue,MaxEffervescence,
                                  Elevation,CarbonateStage,BioticCrustClass,
                                  PedonDepth,Depth200,DepthClass,SlopeShape) )
  
write.csv(Sub,file="F:/Soils/SoilSubset.csv", row.names=TRUE)



















pairs(~DWAClay+DWASand+DWApH+H1.DWA_AWC,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="DWA")
pairs(~H1.ClayPercent+H1.SandPercent+H1.Texture,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")
pairs(~H1.SandSize+H1.pH+H1.EfferScale+H1.MoistRed,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")
pairs(~H1.MoistCClass+H1.MoistValue+Tot.MoistRed,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")
pairs(~Tot.MoistCClass+Tot.MoistValue+MaxEffervescence,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")
pairs(~Elevation+CarbonateStage+BioticCrustClass,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")
pairs(~PedonDepth+Depth200+DepthClass+SlopeShape,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")


pairs(~H1.Texture+H1.ClayPercent+H1.SandPercent,data=Sub, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="")










  
# install.packages("Hmisc")
library(Hmisc)
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
z <- subset(Sub, select = -c(SlopeShape,DepthClass,H1.Texture,H1.SandSize) )
res<-rcorr(as.matrix(z[,1:20]), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]















den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)
dena <- den[c(38:136),"ARTR2"]
denu <- den[,"ARTR2"]
artru <- cbind(soil,denu)
artra <- cbind(soila,dena)
  
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
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All Clay Variables")
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Clay Variables")



# Sand
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All Sand Variables")
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Sand Variables")


# pH
pairs(~MaxpH+DWApH+H1.pH+pH.50+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All pH Variables")
pairs(~MaxpH+DWApH+H1.pH+pH.50+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April pH Variables")


# AWHC
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All AWHC Variables")
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April AWHC Variables")


# Effervescence
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All Effervescence Variables")
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Effervescence Variables")


# CClass
pairs(~Tot.DryCClass+H1.DryCClass+Tot.MoistCClass+H1.MoistCClass+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="ALL CClass")
pairs(~Tot.DryCClass+H1.DryCClass+Tot.MoistCClass+H1.MoistCClass+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April CClass")


# Red
pairs(~Tot.DryRed+H1.DryRed+Tot.MoistRed+H1.MoistRed+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="ALL Red")
pairs(~Tot.DryRed+H1.DryRed+Tot.MoistRed+H1.MoistRed+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Red")


# DryValue
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All DryValue Variables")
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April DryValue Variables")

# Value
pairs(~Tot.DryValue+H1.DryValue+Tot.MoistValue+H1.MoistValue+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All Value Variables")
pairs(~Tot.DryValue+H1.DryValue+Tot.MoistValue+H1.MoistValue+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Value Variables")


# Depth
pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All Depth Variables")
pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Depth Variables")

# Factors
pairs(~SlopeShape+Tot.Texture+Tot.SandSize+H1.Texture+H1.SandSize+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="All Factor Variables")
pairs(~SlopeShape+Tot.Texture+Tot.SandSize+H1.Texture+H1.SandSize+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Factor Variables")







# Clay, Sand, pH
pairs(~DWAClay+H1.ClayPercent+DWASand+H1.SandPercent+DWApH+H1.pH+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="ALL Soil")

pairs(~DWAClay+H1.ClayPercent+DWASand+H1.SandPercent+DWApH+H1.pH+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April Soil")

# Moist Red,CClass,Value
pairs(~Tot.MoistRed+H1.MoistRed+Tot.MoistCClass+H1.MoistCClass+Tot.MoistValue+H1.MoistValue+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="ALL MoistColor")

pairs(~Tot.MoistRed+H1.MoistRed+Tot.MoistCClass+H1.MoistCClass+Tot.MoistValue+H1.MoistValue+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April MoistColor")

# Dry Red,CClass,Value
pairs(~Tot.DryRed+H1.DryRed+Tot.DryCClass+H1.DryCClass+Tot.DryValue+H1.DryValue+DryValue.50+MaxDryValue+denu,data=artru, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="ALL DryColor")

pairs(~Tot.DryRed+H1.DryRed+Tot.DryCClass+H1.DryCClass+Tot.DryValue+H1.DryValue+DryValue.50+MaxDryValue+dena,data=artra, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="April DryColor")
