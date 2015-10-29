den <- read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE, row.names=1)

ARTR2 <- den$ARTR2

# log transform 
log <- log(ARTR2)
log[mapply(is.infinite, log)] <- 0

# log10 transform 
log10 <- log10(ARTR2)
log10[mapply(is.infinite, log10)] <- 0

# log(x+1) transform 
log1 <- log(ARTR2+1)

# Square Root transform 
sqrt <- sqrt(ARTR2)

# Cube Root transform 
cube <- (ARTR2)^(1/3)

# log(x+c) transform 
logc <- log(ARTR2+.000000001)

dpois(ARTR2, lambda=1, lower.tail=F)


soil <- read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)

a <- cbind(soil,ARTR2)
a <- a[, !sapply(a, is.factor)]

b <- cbind(soil,log)
b <- b[, !sapply(b, is.factor)]

c <- cbind(soil,log10)
c <- c[, !sapply(c, is.factor)]

d <- cbind(soil,log1)
d <- d[, !sapply(d, is.factor)]

e <- cbind(soil,sqrt)
e <- e[, !sapply(e, is.factor)]

f <- cbind(soil,cube)
f <- f[, !sapply(f, is.factor)]

g <- cbind(soil,logc)
g <- g[, !sapply(g, is.factor)]


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
# LogClay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+log,data=b, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log Clay Variables")
# Log10Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+log10,data=c, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log 10 Clay Variables")
# Log(x+1)Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+log1,data=d, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log(x+1)Clay Variables")
# Square Root
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+sqrt,data=e, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Square Root Clay Variables")
# Cube Root
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+cube,data=f, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Cube Root Clay Variables")
# Log(x+c)Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+logc,data=g, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Log(x+c)Clay Variables")


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



# 
pairs(~H1.SandPercent+DWASand+DWA.AWHC+MaxAWHC+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.SandPercent+H1.ClayPercent+H1.Depth+H1.DWA_AWC+H1.pH+ARTR2,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+Sand.50,data=a, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50
MaxSand+DWASand+H1.SandPercent+Sand.50