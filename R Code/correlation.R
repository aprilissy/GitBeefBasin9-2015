u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
p<-read.csv("F:/Soils/SoilEnvironmentaldataNSplain.csv",header=TRUE, row.names=1)
a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)

z <- subset(u, select = -c(SlopeShape,DepthClass,H1.Texture,H1.SandSize,Tot.Texture,Tot.SandSize) )
pearson <- cor(z, method = c("pearson"),use = "complete.obs")
kendall <- cor(z, method = c("kendall"),use = "complete.obs")
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
res<-rcorr(as.matrix(z[,1:42]), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)
q <- symnum(spearman)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]

# Pinsig <- corPval[ which(corPval$p > 0.05), ]
# Psig <- corPval[ which(corPval$p < 0.05),]
# 
# CorSpos <- corPval[ which((corPval$cor >= 0.00)&(corPval$cor <= 0.29999)),]
# CorSneg <- corPval[ which((corPval$cor < 0.00)&(corPval$cor >= -0.29999)),]
# CorS <- rbind(CorSpos,CorSneg)
# 
# CorMpos <- corPval[ which((corPval$cor > 0.29999)&(corPval$cor <= 0.49999)),]
# CorMneg <- corPval[ which((corPval$cor < -0.29999)&(corPval$cor >= -0.49999)),]
# CorM <- rbind(CorMpos,CorMneg)
# 
# CorLpos <- corPval[ which(corPval$cor > .49999),]
# CorLneg <- corPval[ which(corPval$cor < -0.49999),]
# CorL <- rbind(CorLpos,CorLneg)
# 


# install.packages("corrplot")
library(corrplot)
corrplot(spearman, type="upper", order="hclust", tl.col="black", tl.srt=45)

corrplot(spearman,type="upper",order = "AOE", cl.pos = "b", tl.pos = "d", tl.srt = 60)
