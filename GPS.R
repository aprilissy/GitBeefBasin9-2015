library(tidyr)
library(plyr)

###############################
# GPS Points

# read in gps data. Don't do ,row.names=1 because we need to be able to edit the id character strings.
gps <- read.csv("F:/USGSPoints_Check/GPSpoints.csv", header = T,nrows = 444)

# extract USGS points.
U <- gps[c(100:164),]
U$Plot.Name <- extract_numeric(U$Plot.Name) # removes CLHS and P leaving only numbers.
rownames(U) <- U[,1] ; U <- U[,-1] # Set row.names.
U <- U[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),]

# extract April points
A <- gps[c(1:99),] 
A$Plot.Name <- as.character(A$Plot.Name) # Set as character so we can edit in the following line.
A$Plot.Name = substr(A$Plot.Name,1,nchar(A$Plot.Name)-1) # removes P at end of plot id.
A$Plot.Name <- gsub("-", "_", A$Plot.Name) # changes - to _ for plot id.
rownames(A) <- A[,1] ; A <- A[,-1] # Set row.names.

# combine USGS and April points.
gps <- rbind(A,U)
gps <- gps[ order(row.names(gps)), ]  #sort a dataframe by the order of the elements in row.names


###############################
# Sagebrush Data

sage <- read.csv("F:/ShrubDensity/PresenceAbsence/Output/USGSplotXspp.csv", header = T,nrows = 444)
rownames(sage) <- sage[,1] ; sage <- sage[,-1] # Set row.names.
Asage <- sage[c(61:159),c(8:9)]
Usage <- sage[c("1","2","10","11","12","14","15","16","17","18","19","20","21","23","24","32","33","38","39","40","42","43","44","47","48","50","57","59","60","61","67","68","73","77","80","82","90"),c(8:9)]
sage <- rbind(Asage,Usage)
sage <- sage[ order(row.names(sage)), ]  #sort a dataframe by the order of the elements in row.names


###############################
# Soil Data

soil <- read.csv("F:/Soils/SoilEnvironmentaldataUSGSApril.csv", header = T,nrows = 444)
soil <- subset(soil, select = c(id,Elevation,PedonDepth,MaxSand,MaxpH,MaxDryValue) )
rownames(soil) <- soil[,1] ; soil <- soil[,-1] # Set row.names.
soil <- soil[ order(row.names(soil)), ]  #sort a dataframe by the order of the elements in row.names

###############################
# Combine GPS, Sage, and Soils

map <- cbind(sage,gps,soil)
write.csv(map,file="F:/AprilsData.csv")
