# Sagebrush Count by Transect
library(plyr)

##########################  LPI   ####################################

# read in all LPI data by transect
lpi <- read.csv("F:/LPI/USGSLPIofAprilAndUSGS.csv", header=T) 
lpi <- lpi[,-c(1,2,6,7,9)] # remove extraneous columns
table(lpi$Indicator)

# break up in to live and dead sagebrush
# Live
lpi.l <- subset(lpi, Indicator %in% "ARTR2") # select only rows with ARTR2
dp <- subset(lpi, Indicator %in% "ARTR2/DP")# select only rows with ARTR2/DP
lpi.l <- merge(lpi.l,dp,by=c("Plot","Line")) #Combine ARTR2 and ARTR2/DP
lpi.l$total <- lpi.l$Any.Hit.N.x + lpi.l$Any.Hit.N.y # add ARTR and ARTR/DP totals
lpi.l <- lpi.l[-c(1,2,3),c(1,2,3,7)] # remove extra columns and usgs1 (no artr, not in density)
lpi.l <- rename(lpi.l, replace = c("Indicator.x" = "species")) # rename species column
lpi.l <- lpi.l[order(lpi.l$Plot),]  #sort a dataframe by the order of the elements in Plot
rownames(lpi.l) <- NULL # remove extra row.names column
lpi.l.m<-xtabs(total~Plot+Line, lpi.l) # put in plot by transect matrix
write.csv(lpi.l.m,file="F:/SagebrushCount/lpi.l.csv")

# Dead
lpi.d <- subset(lpi, Indicator %in% "ARTR2/D") # select only rows with ARTR2/D
lpi.d <- rename(lpi.d, replace = c("Indicator" = "species", "Any.Hit.N" = "total")) # rename species column
lpi.d <- lpi.d[order(lpi.d$Plot),]  #sort a dataframe by the order of the elements in SpeciesCode
lpi.d <- lpi.d[-c(1,2,3),] # remove usgs1 (no artr, not in density)
rownames(lpi.d) <- NULL # remove extra row.names column
lpi.d.m<-xtabs(total~Plot+Line, lpi.d) # put in plot by transect matrix
write.csv(lpi.d.m,file="F:/SagebrushCount/lpi.d.csv")


#######################   Density   ############################

# read in all Density data by transect
den <- read.csv("F:/SagebrushCount/PlantDenSpeciesSummary 8-21AddLPImisses.csv", header=T) 
# break up into live and dead sagebrush
den.l <- subset(den, Species %in% "ARTR2") # select only rows with ARTR2
rownames(den.l) <- NULL # remove extra row.names column
den.d <- subset(den, Species %in% "ARTR2/D") # select only rows with ARTR2/D
rownames(den.d) <- NULL # remove extra row.names column

# plot by transect
plotxtransect<-xtabs(Total~Plot+Line, den.l) # put in plot by spp matrix
write.csv(plotxtransect,file="F:/SagebrushCount/den.l.plotxtransect.csv")
plotxtransect<-xtabs(Total~Plot+Line, den.d) # put in plot by spp matrix
write.csv(plotxtransect,file="F:/SagebrushCount/den.d.plotxtransect.csv")


#################### Combine LPI & Density ######################
### Live
# read in lpi and density data
# seperate usgs and april data

l.lpi <- read.csv("F:/SagebrushCount/lpi.l.csv", header=T, row.names = 1) 
a.l.lpi <- l.lpi[c(61:159),] # keep only april data
u.l.lpi <- l.lpi[c(1:60),] # keep only usgs data

l.den <- read.csv("F:/SagebrushCount/den.l.plotxtransect.csv", header=T, row.names = 1) 
a.l.den <- l.den[c(61:159),] # keep only april data
u.l.den <- l.den[c(1:60),] # keep only usgs data

# combine lpi and density data
# rename columns
l <- merge(a.l.lpi,a.l.den,by="row.names") #Combine live lpi and density
l$T1 <- l$X1.x + l$X1.y # add lpi and density totals
l$T2 <- l$X2.y
l$T3 <- l$X3.y
l$T4 <- l$X4.y
l$T5 <- l$X5.x + l$X5.y # add lpi and density totals
l <- l[,-c(2:11)] # remove extra columns
rownames(l) <- l$Row.names
l$Row.names <- NULL

# live/live transect totals
l.relcover <- (l/rowSums(l))*100
write.csv((format(l.relcover, digits=2)),file="F:/SagebrushCount/lrelcover.csv")


### Dead
# read in lpi and density data
# seperate usgs and april data

d.lpi <- read.csv("F:/SagebrushCount/lpi.d.csv", header=T, row.names = 1) 
a.d.lpi <- d.lpi[c(61:159),] # keep only april data
u.d.lpi <- d.lpi[c(1:60),] # keep only usgs data

d.den <- read.csv("F:/SagebrushCount/den.d.plotxtransect.csv", header=T, row.names = 1) 
a.d.den <- d.den[c(61:159),] # keep only april data
u.d.den <- d.den[c(1:60),] # keep only usgs data

# combine lpi and density data
# rename columns
d <- merge(a.d.lpi,a.d.den,by="row.names") #Combine dead lpi and density
d$T1 <- d$X1.x + d$X1.y # add lpi and density totals
d$T2 <- d$X2.y
d$T3 <- d$X3.y
d$T4 <- d$X4.y
d$T5 <- d$X5.x + d$X5.y # add lpi and density totals
d <- d[,-c(2:11)] # remove extra columns
rownames(d) <- d$Row.names
d$Row.names <- NULL

# dead/dead transect totals
d.relcover <- (d/rowSums(d))*100
write.csv((format(d.relcover, digits=2)),file="F:/SagebrushCount/drelcover.csv")

# Combine live and dead into one document
ld.rel <- merge(l.relcover,d.relcover,by="row.names") #Combine live and dead data
names(ld.rel) <- c("Plot","T1.L","T2.L","T3.L","T4.L","T5.L","T1.D","T2.D","T3.D","T4.D","T5.D")
write.csv((format(ld.rel, digits =3)),file="F:/SagebrushCount/ldpercentspp.csv", row.names=FALSE)

################# Combine Live and Dead ######################

ld <- merge(l,d,by="row.names") #Combine live and dead data
rownames(ld) <- ld$Row.names
ld$Row.names <- NULL

relcover <- (ld/rowSums(ld))*100
write.csv((format(relcover, digits=2)),file="F:/SagebrushCount/ldrelcover.csv")

ld$Live <- ld$T1.x + ld$T2.x + ld$T3.x + ld$T4.x + ld$T5.x # add lpi and density totals
ld$Total <- ld$T1.x + ld$T2.x + ld$T3.x + ld$T4.x + ld$T5.x + 
            ld$T1.y + ld$T2.y + ld$T3.y + ld$T4.y + ld$T5.y
ld$LiveTotal <- (ld$Live/ld$Total)*100
ld$Dead <- ld$T1.y + ld$T2.y + ld$T3.y + ld$T4.y + ld$T5.y # add lpi and density totals
ld$DeadTotal <- (ld$Dead/ld$Total)*100

percent <- merge(relcover,ld,by="row.names") #Combine live and dead data
percent <- percent[,-c(12:23,25)] # remove extra columns
names(percent) <- c("Plot","T1.L","T2.L","T3.L","T4.L","T5.L","T1.D","T2.D","T3.D","T4.D","T5.D","LiveTotal","DeadTotal")
write.csv((format(percent, digits =3)),file="F:/SagebrushCount/ldpercent.csv", row.names=FALSE)


################## Total live #################################
################## Total dead #################################
############# Percent Live Plot Level #########################

## LPI

l.lpi <- read.csv("F:/SagebrushCount/lpi.l.csv", header=T, row.names = 1) 
l.lpi$LPI.LiveCount <- rowSums(l.lpi)
d.lpi <- read.csv("F:/SagebrushCount/lpi.d.csv", header=T, row.names = 1) 
l.lpi$LPI.DeadCount <- rowSums(d.lpi)
l.lpi <- l.lpi[,-c(1:5)] # remove extra columns
l.lpi$LPI.PctLive <- (l.lpi$LPI.LiveCount/rowSums(l.lpi))*100

## Density

l.den <- read.csv("F:/SagebrushCount/den.l.plotxtransect.csv", header=T, row.names = 1) 
l.den$DEN.LiveCount <- rowSums(l.den)
d.den <- read.csv("F:/SagebrushCount/den.d.plotxtransect.csv", header=T, row.names = 1) 
l.den$DEN.DeadCount <- rowSums(d.den)
l.den <- l.den[,-c(1:5)] # remove extra columns
l.den$DEN.PctLive <- (l.den$DEN.LiveCount/rowSums(l.den))*100

## Combine

geno <- merge(l.lpi,l.den,by="row.names") #Combine live and dead data
write.csv((format(geno, digits =1)),file="F:/SagebrushCount/geno.csv", row.names=FALSE)
