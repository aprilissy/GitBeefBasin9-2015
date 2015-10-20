rel<-read.csv("F:/LPI/AprilLPIRelativeCoverCommonInExcel.csv",header=TRUE, row.names=1)
den<-read.csv("F:/LPI/AprilLPIplotXspp.csv",header=TRUE, row.names=1)
den <- den/150
write.csv(den,file="F:/LPI/AprilLPIDenM2.csv", row.names=TRUE)


plot(rel$ARTR2, rel$SPCR, col='navyblue')
plot(den$ARTR2, den$SPCR, col='darkgreen')

plot(rel$SPCR)
plot(den$SPCR)

plot(rel$BOGR, rel$SPCR, col='navyblue')
plot(den$BOGR, den$SPCR, col='darkgreen')

plot(rel$ARTR2, rel$BOGR, col='navyblue')
plot(den$ARTR2, den$BOGR, col='darkgreen')

plot(rel$BOGR)
plot(den$BOGR)


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
points(rel$BOGR, den$BOGR, col='navyblue')
points(rel$BOGR.D, den$BOGR.D, col='red')




u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
p<-read.csv("F:/Soils/SoilEnvironmentaldataNSplain.csv",header=TRUE, row.names=1)
a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)

plot(u$MaxClay, u$Clay.50, col='blue', main='USGS')
plot(p$MaxClay, p$Clay.50, col='blue', main='Plains')
plot(a$MaxClay, a$Clay.50, col='blue', main='April')

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
