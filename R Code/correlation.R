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
library(graphics)
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

write.csv(Susan7,file="F:/Soil.7SpearmansCorrelation.csv", row.names=TRUE)












Den <- read.csv("F:/LPI/Output/AprilLPIDensityM2.csv",header=TRUE, row.names=1)
ARTR2 <- Den$ARTR2
KRLA2 <- Den$KRLA2
ATCA2 <- Den$ATCA2
BOGR2 <- Den$BOGR2
SPCR <- Den$SPCR
HECO26 <- Den$HECO26
Bare.Soil <- Den$Bare.Soil

# ARTR2
artr <- cbind(a,ARTR2) ; artr <- artr[, !sapply(artr, is.factor)] # Combine ARTR with Soils, remove the factor variables
spearman <- cor(artr, method = c("spearman"),use = "complete.obs")

res<-rcorr(as.matrix(artr), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
ARTR2 <- spearman[43,]; ARTR2 <- ARTR2[order(-abs(ARTR2))]
ARTR2 <-t(ARTR2); ARTR2<-t(ARTR2)
write.csv(ARTR2,file="F:/SoilARTR2SpearmansCorrelation.csv", row.names=TRUE)

# KRLA2
krla <- cbind(a,KRLA2) ; krla <- krla[, !sapply(krla, is.factor)] # Combine ARTR with Soils, remove the factor variables
spearman <- cor(krla, method = c("spearman"),use = "complete.obs")

res<-rcorr(as.matrix(krla), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
KRLA2 <- spearman[43,]; KRLA2 <- KRLA2[order(-abs(KRLA2))]
KRLA2 <-t(KRLA2); KRLA2<-t(KRLA2)
write.csv(KRLA2,file="F:/SoilKRLA2SpearmansCorrelation.csv", row.names=TRUE)

# ATCA2
atca <- cbind(a,ATCA2) ; atca <- atca[, !sapply(atca, is.factor)] # Combine ARTR with Soils, remove the factor variables
spearman <- cor(atca, method = c("spearman"),use = "complete.obs")

res<-rcorr(as.matrix(atca), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
ATCA2 <- spearman[43,]; ATCA2 <- ATCA2[order(-abs(ATCA2))]
ATCA2 <-t(ATCA2); ATCA2<-t(ATCA2)
write.csv(ATCA2,file="F:/SoilATCA2SpearmansCorrelation.csv", row.names=TRUE)

# BOGR2
bogr <- cbind(a,BOGR2) ; bogr <- bogr[, !sapply(bogr, is.factor)] # Combine ARTR with Soils, remove the factor variables
spearman <- cor(bogr, method = c("spearman"),use = "complete.obs")

res<-rcorr(as.matrix(bogr), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
BOGR2 <- spearman[43,]; BOGR2 <- BOGR2[order(-abs(BOGR2))]
BOGR2 <-t(BOGR2); BOGR2<-t(BOGR2)
write.csv(BOGR2,file="F:/SoilBOGR2SpearmansCorrelation.csv", row.names=TRUE)

# SPCR
spcr <- cbind(a,SPCR) ; spcr <- spcr[, !sapply(spcr, is.factor)] # Combine ARTR with Soils, remove the factor variables
spearman <- cor(spcr, method = c("spearman"),use = "complete.obs")

res<-rcorr(as.matrix(spcr), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
SPCR <- spearman[43,]; SPCR <- SPCR[order(-abs(SPCR))]
SPCR <-t(SPCR); SPCR<-t(SPCR)
write.csv(SPCR,file="F:/SoilSPCRSpearmansCorrelation.csv", row.names=TRUE)

# HECO26
heco26 <- cbind(a,HECO26) ; heco26 <- heco26[, !sapply(heco26, is.factor)] # Combine ARTR with Soils, remove the factor variables
spearman <- cor(heco26, method = c("spearman"),use = "complete.obs")

res<-rcorr(as.matrix(heco26), type=c("spearman"))
corPval <- flattenCorrMatrix(res$r, res$P)

Susan7 <- corPval[ which((corPval$cor >= 0.7)|(corPval$cor < -0.7)),]
Susan8 <- corPval[ which((corPval$cor >= 0.8)|(corPval$cor < -0.8)),]
HECO26 <- spearman[43,]; HECO26 <- HECO26[order(-abs(HECO26))]
HECO26 <-t(HECO26); HECO26<-t(HECO26)
write.csv(HECO26,file="F:/SoilHECO26SpearmansCorrelation.csv", row.names=TRUE)


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



# Clay & Hue
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+H1.MoistRed+Tot.MoistRed+H1.DryRed+Tot.DryRed+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay & Hue Variables")

# Clay & Sand
pairs(~DWAClay+H1.ClayPercent+DWASand+H1.SandPercent+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay & Sand Variables")

# Clay
pairs(~MaxClay+DWAClay+H1.ClayPercent+Clay.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Clay Variables")


# Sand
pairs(~MaxSand+DWASand+H1.SandPercent+Sand.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Sand Variables")


# pH
pairs(~MaxpH+DWApH+H1.pH+pH.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="pH Variables")


# AWHC
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="AWHC Variables")


# Effervescence
pairs(~MaxEffervescence+Tot.EfferScale+H1.EfferScale+EfferScale.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="Effervescence Variables")


# DryValue
pairs(~MaxDryValue+Tot.DryValue+H1.DryValue+DryValue.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="DryValue Variables")


# art <- cbind(a,HECO26)
# # Depth
# pairs(~PedonDepth+Depth200+H1.Depth+DepthClass+HECO26,data=art, 
#       lower.panel=panel.smooth, upper.panel=panel.cor, 
#       pch=20, na.action = na.exclude, main="Depth Variables")
# 
# # Factors
# pairs(~SlopeShape+Tot.Texture+Tot.SandSize+H1.Texture+H1.SandSize+HECO26,data=art, 
#       lower.panel=panel.smooth, upper.panel=panel.cor, 
#       pch=20, na.action = na.exclude, main="Factor Variables")
# 

# 
pairs(~H1.SandPercent+DWASand+DWA.AWHC+MaxAWHC+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.SandPercent+H1.ClayPercent+H1.Depth+H1.DWA_AWC+H1.pH+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+Sand.50,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~MaxAWHC+DWA.AWHC+H1.DWA_AWC+AWHC.50+MaxSand+DWASand+H1.SandPercent+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Variables")

# 
pairs(~H1.DryRed+Tot.DryRed+H1.MoistRed+Tot.MoistRed+H1.EfferScale+Tot.EfferScale+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Hue Variables")

# 
pairs(~H1.DryCClass+Tot.DryCClass+H1.MoistCClass+Tot.MoistCClass+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Chroma Variables")

# 
pairs(~H1.DryValue+Tot.DryValue+H1.MoistValue+Tot.MoistValue+MaxDryValue+DryValue.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main=" Value Variables")

# 
pairs(~H1.DryValue+H1.EfferScale+Tot.DryValue+H1.pH+DWApH+Tot.DryCClass+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="  Variables")

# 
pairs(~MaxEffervescence+Tot.DryValue+H1.MoistValue+Tot.MoistValue+MaxDryValue+DryValue.50+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="  Variables")

# 
pairs(~MaxEffervescence+H1.EfferScale+Tot.DryValue+H1.pH+DWApH+Tot.DryCClass+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="  Variables")

# 
pairs(~BioticCrustClass+DryValue.50+Tot.MoistRed+H1.DryValue+Tot.DryRed+Elevation+HECO26,data=heco26, 
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, na.action = na.exclude, main="  Variables")





#this next command defines a new function which can then be used 
#for making multiple histograms 

multi.hist <- function(x) {nvar <- dim(x)[2]  #number of variables
                           nsize=trunc(sqrt(nvar))+1   #size of graphic
                           old.par <- par(no.readonly = TRUE) # all par settings which can be changed
                           par(mfrow=c(nsize,nsize))       #set new graphic parameters
                           for (i in 1:nvar) {
                             name=names(x)[i]                #get the names for the variables
                             hist(x[,i],main=name,xlab=name) }  #draw the histograms for each variable
                           on.exit(par(old.par))   #set the graphic parameters back to the original
}

#now use the function on the data
a<-artr[,c(1:15)]
b<-artr[,c(16:30)]
c<-artr[,c(30:42)]

multi.hist(a)   #draw the histograms for all variables  (see above)

