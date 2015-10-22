rel<-read.csv("F:/LPI/Output/AprilLPIRelativeCover.csv",header=TRUE, row.names=1)
den<-read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE, row.names=1)


reg1 <- lm(write~read)
par(cex=.8)
plot(read, write)
abline(reg1)

reg1 <- lm(rel$ARTR2~den$ARTR2)
par(cex=.8)
plot(rel$ARTR2,den$ARTR2)
abline(reg1)

# Basic Scatterplot Matrix
pairs(~ARTR2+ARTR2.D+ATCA2+ATCA2.D+KRLA2+BOGR2+BOGR2.D,data=rel, 
      main="Simple Scatterplot Matrix")

plot(rel$ARTR2, rel$SPCR, col='navyblue')
plot(den$ARTR2, den$SPCR, col='darkgreen')

plot(rel$SPCR)
plot(den$SPCR)

plot(rel$BOGR2, rel$SPCR, col='navyblue')
plot(den$BOGR2, den$SPCR, col='darkgreen')

plot(rel$ARTR2, rel$BOGR2, col='navyblue')
plot(den$ARTR2, den$BOGR2, col='darkgreen')

plot(rel$BOGR2)
plot(den$BOGR2)


plot(rel$ARTR2, rel$KRLA2, col='navyblue')
plot(den$ARTR2, den$KRLA2, col='darkgreen')

plot(rel$KRLA2)
plot(den$KRLA2)


plot(rel$ARTR2, rel$ATCA2, col='navyblue')
plot(den$ARTR2, den$ATCA2, col='darkgreen')

plot(rel$ATCA2)
plot(den$ATCA2)


plot(rel$KRLA2, rel$ATCA2, col='navyblue')
plot(rel$ATCA2, rel$KRLA2, col='navyblue')
plot(den$KRLA2, den$ATCA2, col='darkgreen')
plot(den$ATCA2, den$KRLA2, col='darkgreen')



plot(rel$ARTR2, den$ARTR2, col='navyblue')
plot(rel$ARTR2.D, den$ARTR2.D, col='darkgreen')

plot(rel$ARTR2, den$ARTR2, col='navyblue')
points(rel$ARTR2.D, den$ARTR2.D, col='darkgreen')

plot(rel$HECO26, den$HECO26, col='navyblue')
points(rel$HECO26.D, den$HECO26.D, col='darkgreen')


plot(rel$SPCR, den$SPCR, col='darkgreen')
points(rel$SPCR.D, den$SPCR.D, col='navyblue')
plot(rel$SPCR.D, den$SPCR.D, col='navyblue')



plot(rel$ACHY, den$ACHY, col='darkgreen')
points(rel$AMID, den$AMID, col='navyblue')
points(rel$AMSIN, den$AMSIN, col='red')
points(rel$ARPU9, den$ARPU9, col='purple')
points(rel$ARTR2, den$ARTR2, col='green')
points(rel$ARTR2.D, den$ARTR2.D, col='blue')
points(rel$ATCA2, den$ATCA2, col='green4')
points(rel$ATCA2.D, den$ATCA2.D, col='orange')
points(rel$BOGR2, den$BOGR2, col='navyblue')
points(rel$BOGR2.D, den$BOGR2.D, col='red')




u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
p<-read.csv("F:/Soils/SoilEnvironmentaldataNSplain.csv",header=TRUE, row.names=1)
a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)




# Basic Scatterplot Matrix
pairs(~Elevation+Aspect+SlopeShape+Slope,data=u, 
      main="Simple Scatterplot Matrix")


plot(u$Elevation~u$Aspect, col='blue', main='USGS')
abline(lm(u$Elevation~u$Aspect), col="red") # regression line (y~x) 

plot(u$Elevation~u$Slope, col='blue', main='USGS')
abline(lm(u$Elevation~u$Slope), col="red") # regression line (y~x) 

plot(u$Elevation~u$CarbonateStage, col='blue', main='USGS')
abline(lm(u$Elevation~u$CarbonateStage), col="red") # regression line (y~x) 

plot(u$Elevation~u$BioticCrustClass, col='blue', main='USGS')
abline(lm(u$Elevation~u$BioticCrustClass), col="red") # regression line (y~x) 

plot(u$Elevation~u$PedonDepth, col='blue', main='USGS')
abline(lm(u$Elevation~u$PedonDepth), col="red") # regression line (y~x) 

plot(u$Elevation~u$Depth200, col='blue', main='USGS')
abline(lm(u$Elevation~u$Depth200), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.DryRed, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.DryRed), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.DryValue, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.DryValue), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.DryCClass, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.DryCClass), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.MoistRed, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.MoistRed), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.MoistValue, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.MoistValue), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.MoistCClass, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.MoistCClass), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.SandPercent, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.SandPercent), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.ClayPercent, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.ClayPercent), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.pH, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.pH), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.EfferScale, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.EfferScale), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.Depth, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.Depth), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.DWA_AWC, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.DWA_AWC), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.DryRed, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.DryRed), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.DryValue, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.DryValue), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.DryCClass, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.DryCClass), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.MoistRed, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.MoistRed), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.MoistValue, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.MoistValue), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.MoistCClass, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.MoistCClass), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.EfferScale, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.EfferScale), col="red") # regression line (y~x) 

plot(u$Elevation~u$DWAClay, col='blue', main='USGS')
abline(lm(u$Elevation~u$DWAClay), col="red") # regression line (y~x) 

plot(u$Elevation~u$DWASand, col='blue', main='USGS')
abline(lm(u$Elevation~u$DWASand), col="red") # regression line (y~x) 

plot(u$Elevation~u$DWApH, col='blue', main='USGS')
abline(lm(u$Elevation~u$DWApH), col="red") # regression line (y~x) 

plot(u$Elevation~u$DWA.AWHC, col='blue', main='USGS')
abline(lm(u$Elevation~u$DWA.AWHC), col="red") # regression line (y~x) 

plot(u$Elevation~u$Sand.50, col='blue', main='USGS')
abline(lm(u$Elevation~u$Sand.50), col="red") # regression line (y~x) 

plot(u$Elevation~u$Clay.50, col='blue', main='USGS')
abline(lm(u$Elevation~u$Clay.50), col="red") # regression line (y~x) 

plot(u$Elevation~u$pH.50, col='blue', main='USGS')
abline(lm(u$Elevation~u$pH.50), col="red") # regression line (y~x) 

plot(u$Elevation~u$DryValue.50, col='blue', main='USGS')
abline(lm(u$Elevation~u$DryValue.50), col="red") # regression line (y~x) 

plot(u$Elevation~u$EfferScale.50, col='blue', main='USGS')
abline(lm(u$Elevation~u$EfferScale.50), col="red") # regression line (y~x) 

plot(u$Elevation~u$AWHC.50, col='blue', main='USGS')
abline(lm(u$Elevation~u$AWHC.50), col="red") # regression line (y~x) 

plot(u$Elevation~u$MaxSand, col='blue', main='USGS')
abline(lm(u$Elevation~u$MaxSand), col="red") # regression line (y~x) 

plot(u$Elevation~u$MaxClay, col='blue', main='USGS')
abline(lm(u$Elevation~u$MaxClay), col="red") # regression line (y~x) 

plot(u$Elevation~u$MaxpH, col='blue', main='USGS')
abline(lm(u$Elevation~u$MaxpH), col="red") # regression line (y~x) 

plot(u$Elevation~u$MaxDryValue, col='blue', main='USGS')
abline(lm(u$Elevation~u$MaxDryValue), col="red") # regression line (y~x) 

plot(u$Elevation~u$MaxEffervescence, col='blue', main='USGS')
abline(lm(u$Elevation~u$MaxEffervescence), col="red") # regression line (y~x) 

plot(u$Elevation~u$MaxAWHC, col='blue', main='USGS')
abline(lm(u$Elevation~u$MaxAWHC), col="red") # regression line (y~x) 

plot(u$Elevation~u$SlopeShape, col='blue', main='USGS')
abline(lm(u$Elevation~u$SlopeShape), col="red") # regression line (y~x) 

plot(u$Elevation~u$DepthClass, col='blue', main='USGS')
abline(lm(u$Elevation~u$DepthClass), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.Texture, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.Texture), col="red") # regression line (y~x) 

plot(u$Elevation~u$H1.SandSize, col='blue', main='USGS')
abline(lm(u$Elevation~u$H1.SandSize), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.Texture, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.Texture), col="red") # regression line (y~x) 

plot(u$Elevation~u$Tot.SandSize, col='blue', main='USGS')
abline(lm(u$Elevation~u$Tot.SandSize), col="red") # regression line (y~x) 








plot(u$MaxClay, u$Clay.50, col='blue', main='USGS')
abline(lm(u$MaxClay~u$Clay.50), col="red") # regression line (y~x) 

plot(p$MaxClay, p$Clay.50, col='blue', main='Plains')
abline(lm(p$MaxClay~p$Clay.50), col="red") # regression line (y~x) 

plot(a$MaxClay, a$Clay.50, col='blue', main='April')
abline(lm(a$MaxClay~a$Clay.50), col="red") # regression line (y~x) 

plot(u$MaxSand, u$Sand.50, col='blue', main='USGS')
plot(p$MaxSand, p$Sand.50, col='blue', main='Plains')
plot(a$MaxSand, a$Sand.50, col='blue', main='April')

plot(u$MaxpH, u$pH.50, col='blue', main='USGS')
plot(p$MaxpH, p$pH.50, col='blue', main='Plains')
plot(a$MaxpH, a$pH.50, col='blue', main='April')

plot(u$MaxDryValue, u$DryValue.50, col='blue', main='USGS')
plot(p$MaxDryValue, p$DryValue.50, col='blue', main='Plains')
plot(a$MaxDryValue, a$DryValue.50, col='blue', main='April')

plot(u$MaxEffervescence, u$EfferScale.50, col='blue', main='USGS')
plot(p$MaxEffervescence, p$EfferScale.50, col='blue', main='Plains')
plot(a$MaxEffervescence, a$EfferScale.50, col='blue', main='April')

plot(u$MaxAWHC, u$AWHC.50, col='blue', main='USGS')
plot(p$MaxAWHC, p$AWHC.50, col='blue', main='Plains')
plot(a$MaxAWHC, a$AWHC.50, col='blue', main='April')

plot(u$BioticCrustClass, u$PedonDepth, col='blue', main='USGS')
plot(p$BioticCrustClass, p$PedonDepth, col='blue', main='Plains')
plot(a$BioticCrustClass, a$PedonDepth, col='blue', main='April')


plot(u$pH, u$PedonDepth, col='blue', main='USGS')
plot(p$pH, p$PedonDepth, col='blue', main='Plains')
plot(a$pH, a$PedonDepth, col='blue', main='April')







plot(a$Aspect, a$CarbonateStage, col='blue')

ls(a)

b <- subset(a, select = -c(SlopeShape,DepthClass,H1.Texture,H1.SandSize,Tot.Texture,Tot.SandSize) )
cor <- cor(b, method = c("pearson", "kendall", "spearman"),use = "complete.obs")
pearson <- cor(b, method = c("pearson"),use = "complete.obs")
kendall <- cor(b, method = c("kendall"),use = "complete.obs")
spearman <- cor(b, method = c("spearman"),use = "complete.obs")
write.csv(spearman,file="F:/SpearmanSoilCorrelation.csv", row.names=TRUE)

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
res<-rcorr(as.matrix(b[,1:42]))
corPval <- flattenCorrMatrix(res$r, res$P)
z <- symnum(spearman)

insig <- corPval[ which(corPval$p > 0.05), ]
sig <- corPval[ which(corPval$p < 0.05),]


# install.packages("corrplot")
library(corrplot)
corrplot(spearman, type="upper", order="hclust", tl.col="black", tl.srt=45)

corrplot(spearman,type="upper",order = "AOE", cl.pos = "b", tl.pos = "d", tl.srt = 60)
