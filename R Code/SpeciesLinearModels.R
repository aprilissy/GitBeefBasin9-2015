u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
# p<-read.csv("F:/Soils/SoilEnvironmentaldataNSplain.csv",header=TRUE, row.names=1)
# a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)
# u.count <- u.den*150

ARTR2 <- u ;ARTR2$ARTR2 <- u.den$ARTR2
KRLA2 <- u ;KRLA2$KRLA2 <- u.den$KRLA2
ATCA2 <- u ;ATCA2$ATCA2 <- u.den$ATCA2
BOGR2 <- u ;BOGR2$BOGR2 <- u.den$BOGR2
SPCR <- u ;SPCR$SPCR <- u.den$SPCR
HECO26 <- u ;HECO26$HECO26 <- u.den$HECO26
PG <- u ;PG$PG <- u.den$SPCR + u.den$BOGR2 + u.den$HECO26

u$ARTR2 <- u.den$ARTR2;u$ATCA2 <- u.den$ATCA2;u$KRLA2 <- u.den$KRLA2

ARTR2fit <- lm(as.formula(paste(colnames(ARTR2)[49], "~",
                           paste(colnames(ARTR2)[c(1:48)], collapse = "+"),
                           sep = "")),data=ARTR2)

ATCA2fit <- lm(as.formula(paste(colnames(ATCA2)[49], "~",
                                paste(colnames(ATCA2)[c(1:48)], collapse = "+"),
                                sep = "")),data=ATCA2)

KRLA2fit <- lm(as.formula(paste(colnames(KRLA2)[49], "~",
                                paste(colnames(KRLA2)[c(1:48)], collapse = "+"),
                                sep = "")),data=KRLA2)

BOGR2fit <- lm(as.formula(paste(colnames(BOGR2)[49], "~",
                                paste(colnames(BOGR2)[c(1:48)], collapse = "+"),
                                sep = "")),data=BOGR2)

SPCRfit <- lm(as.formula(paste(colnames(SPCR)[49], "~",
                                paste(colnames(SPCR)[c(1:48)], collapse = "+"),
                                sep = "")),data=SPCR)

HECO26fit <- lm(as.formula(paste(colnames(HECO26)[49], "~",
                                paste(colnames(HECO26)[c(1:48)], collapse = "+"),
                                sep = "")),data=HECO26)

PGfit <- lm(as.formula(paste(colnames(PG)[49], "~",
                                paste(colnames(PG)[c(1:48)], collapse = "+"),
                                sep = "")),data=PG)



# Shrubfit <- lm(as.formula(paste(colnames(u)[cbind(49,50,51)], "~",
#                                 paste(colnames(u)[c(1:48)], collapse = "+"),
#                                 sep = "")),data=u)
# 

(fit <- lm(cbind(ARTR2, ATCA2, KRLA2) ~ PedonDepth + MaxClay + MaxSand + Sand.50 + BioticCrustClass + CarbonateStage + Clay.50 +pH.50, data=u))

#(fit <- lm(cbind(ARTR2, ATCA2, KRLA2) ~as.formula(paste(colnames(u)[c(1:48)], collapse = "+", sep = "")),data=u))


lm.a1 <- lm(ARTR2 ~ ., data = ARTR2)

lm.a2 <- lm(cbind(ARTR2+ATCA2+KRLA2) ~ ., data = u)

summary(lm.a1)
summary(lm.a2$run1)$r.squared
