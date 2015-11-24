Den <- read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE, row.names=1)
Soil <- read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)

ARTR2 <- Den$ARTR2
artr <- cbind(Soil,ARTR2) ; artr <- artr[, !sapply(artr, is.factor)] # Combine ARTR with Soils, remove the factor variables

# ARTR2 + Half of Min Non-Zero
trfHalf <- ARTR2+((min(ARTR2[ARTR2 > 0]))*0.5)
trfHalf <- cbind(Soil,trfHalf); trfHalf <- trfHalf[, !sapply(trfHalf, is.factor)]

# Log ARTR2 + Half of Min Non-Zero
logHalf <- log(ARTR2+((min(ARTR2[ARTR2 > 0]))*0.5))
logHalf <- cbind(Soil,logHalf); logHalf <- logHalf[, !sapply(logHalf, is.factor)]

# log transform 
log <- log(ARTR2)
log[mapply(is.infinite, log)] <- 0
log <- cbind(Soil,log); log <- log[, !sapply(log, is.factor)]

# log10 transform 
log10 <- log10(ARTR2)
log10[mapply(is.infinite, log10)] <- 0
log10 <- cbind(Soil,log10); log10 <- log10[, !sapply(log10, is.factor)]

# log(x+1) transform 
log1 <- log(ARTR2+1)
log1 <- cbind(Soil,log1); log1 <- log1[, !sapply(log1, is.factor)]

# Square Root transform 
sqrt <- sqrt(ARTR2)
sqrt <- cbind(Soil,sqrt);sqrt <- sqrt[, !sapply(sqrt, is.factor)]

# Cube Root transform 
cube <- (ARTR2)^(1/3)
cube <- cbind(Soil,cube); cube <- cube[, !sapply(cube, is.factor)]

# log(x+c) transform 
logc <- log(ARTR2+.000000001)
logc <- cbind(Soil,logc); logc <- logc[, !sapply(logc, is.factor)]


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
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+H1.MoistRed+Tot.MoistRed+H1.DryRed+Tot.DryRed+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay Variables")




# Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay Variables")
# ARTR2 + Half of smallest NonZero value
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+ARTR2,data=trfHalf, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Half NonZero Clay Variables")
# Log of ARTR2 + Half of smallest NonZero value
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+ARTR2,data=logHalf, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="logHalf NonZero Clay Variables")
# LogClay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+log,data=log, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log Clay Variables")
# Log10Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+log10,data=log10, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log 10 Clay Variables")
# Log(x+1)Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+log1,data=log1, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log(x+1)Clay Variables")
# Square Root
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+sqrt,data=sqrt, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Square Root Clay Variables")
# Cube Root
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+cube,data=cube, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Cube Root Clay Variables")
# Log(x+c)Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+logc,data=logc, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log(x+c)Clay Variables")



# Boruta
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Boruta Variables")
# ARTR2 + Half of smallest NonZero value
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+ARTR2,data=trfHalf, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Half NonZero Boruta Variables")
# Log of ARTR2 + Half of smallest NonZero value
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+ARTR2,data=logHalf, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="logHalf NonZero Boruta Variables")
# LogBoruta
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+log,data=log, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log Boruta Variables")
# Log10Boruta
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+log10,data=log10, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log 10 Boruta Variables")
# Log(x+1)Boruta
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+log1,data=log1, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log(x+1)Boruta Variables")
# Square Root
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+sqrt,data=sqrt, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Square Root Boruta Variables")
# Cube Root
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+cube,data=cube, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Cube Root Boruta Variables")
# Log(x+c)Boruta
pairs(~MaxClay+H1.ClayPercent+MaxAWHC+PedonDepth+logc,data=logc, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log(x+c)Boruta Variables")













# Sand
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+ARTR2,data=logHalf, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Sand Variables")


# pH
pairs(~MaxpH+DWApH+H1.pH+pH.50+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="pH Variables")


# AWHC
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="AWHC Variables")


# Effervescence
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Effervescence Variables")


# DryValue
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="DryValue Variables")


art <- cbind(Soil,ARTR2)
# Depth
pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+ARTR2,data=art, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Depth Variables")

# Factors
pairs(~SlopeShape+Tot.Texture+Tot.SandSize+H1.Texture+H1.SandSize+ARTR2,data=art, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Factor Variables")


# 
pairs(~H1.SandPercent+DWASand+DWA.AWHC+MaxAWHC+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.SandPercent+H1.ClayPercent+H1.Depth+H1.DWA_AWC+H1.pH+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+Sand.50,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.DryRed+Tot.DryRed+H1.MoistRed+Tot.MoistRed+H1.EfferScale+Tot.EfferScale+ARTR2,data=artr, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")



MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50
MaxSand+DWASand+H1.SandPercent+Sand.50