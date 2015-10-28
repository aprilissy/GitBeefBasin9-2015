den<-read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE, row.names=1)
a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)
ARTR2 <- den$ARTR2
a <- cbind(a,ARTR2)
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
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay Variables")


# Sand
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Sand Variables")


# pH
pairs(~MaxpH+DWApH+H1.pH+pH.50+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="pH Variables")


# AWHC
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="AWHC Variables")


# Effervescence
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Effervescence Variables")


# DryValue
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="DryValue Variables")

# Depth
pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Depth Variables")
