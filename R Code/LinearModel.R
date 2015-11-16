# http://www.statmethods.net/stats/rdiagnostics.html
library(car)

u<-read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv",header=TRUE, row.names=1)
p<-read.csv("F:/Soils/SoilEnvironmentaldataNSplain.csv",header=TRUE, row.names=1)
a<-read.csv("F:/Soils/SoilEnvironmentaldataApril.csv",header=TRUE, row.names=1)
u.den <- read.csv("F:/LPI/Output/USGSLPIDensityM2.csv",header=TRUE, row.names=1)
u.count <- u.den*150
u$ARTR2 <- u.den$ARTR2
u <- subset(u, select = -c(Sand.50,Clay.50,pH.50,DryValue.50,EfferScale.50,AWHC.50,MaxClay,MaxSand,MaxAWHC,SlopeShape,DepthClass,H1.Texture,H1.SandSize))



fit <- lm(as.formula(paste(colnames(u)[49], "~",
              paste(colnames(u)[c(1:48)], collapse = "+"),
              sep = "")),data=u)

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
avPlots(fit)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(u)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot 
influencePlot(fit,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)

# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)
