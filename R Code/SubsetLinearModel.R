# http://www.statmethods.net/stats/rdiagnostics.html
library(car)

u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
p<-read.csv("F:/Soils/SoilEnvironmentaldataNSplain.csv",header=TRUE, row.names=1)
a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)
u.count <- u.den*150
u$ARTR2 <- u.den$ARTR2

# u <- subset(u, select = -c(Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,DWASand,DWA.AWHC,Tot.Texture,SlopeShape,DepthClass,H1.Texture,H1.SandSize))
u <- subset(u, select = -c(DWAClay,DWASand,DWApH,DWA.AWHC,AWHC.50,Sand.50,H1.Texture,H1.SandSize,Tot.Texture,Tot.SandSize,PedonDepth,Depth200,SlopeShape))



fit <- lm(as.formula(paste(colnames(u)[36], "~",
                           paste(colnames(u)[c(1:35)], collapse = "+"),
                           sep = "")),data=u)

vif(fit) # variance inflation factors 





z <- subset(u, select = -c(DepthClass) )
# pearson <- cor(z, method = c("pearson"),use = "complete.obs")
# kendall <- cor(z, method = c("kendall"),use = "complete.obs")
spearman <- cor(z, method = c("spearman"),use = "complete.obs")
# write.csv(spearman,file="F:/SpearmanSoilCorrelation.csv", row.names=TRUE)

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
res<-rcorr(as.matrix(z[,1:34]), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)
q <- symnum(spearman)


Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
