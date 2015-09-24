# Soil NMDS with USGS data added to April data
data.env.NS <- read.csv("F:/Soils/SoilEnvironmentalDataNSplain.csv",header=TRUE, row.names=1)
data.env.U <- read.csv("F:/Soils/SoilEnvironmentalDataUSGSApril.csv",header=TRUE, row.names=1)


data.env.NS[is.na(data.env.NS)] <- 0 # replace NA with 0
data.env.U[is.na(data.env.U)] <- 0 # replace NA with 0


sage.NS <- read.csv("F:/SageNMDSvariables/Sage.Env.NSplainApril.csv",header=TRUE,row.names=1)
sage.U <- read.csv("F:/SageNMDSvariables/Sage.Env.USGS.csv",header=TRUE,row.names=1)

sage.NS[is.na(sage.NS)] <- 0 # replace NA with 0
sage.U[is.na(sage.U)] <- 0 # replace NA with 0



# # set factors to factor not numeric
# ix <- c(41)
# data.env.A[ix] <- lapply(data.env.A[ix], as.factor) 
# id <- c(1:40,42:43)
# data.env.A[id] <- lapply(data.env.A[id], as.numeric)
# data.A <- data.env.A[,c(1:40,42:43)]




# # look at distance methods
# rankindex(sage.A, data.A, c("euc","man","bray", "jac", "kul"))

#calculate dissimilarities, use function "vegdist"in VEGAN package
# data.dis<-vegdist(data.A,method="bray")
# dis.matrix<-as.matrix(data.dis)
# rankindex(dis.matrix,data.A)




# soil.s <- All.ord$species # Soil variable scores
# plot.s <- All.ord$points # Plot scores
# 
# soil.s <- as.data.frame(soil.s)
# soil.s$soil< - rownames(soil.s)
# soil.s <- cbind(Row.Names = rownames(soil.s), soil.s)
# rownames(soil.s)<-NULL
# 
# library(rgl)
# with(soil.s,plot3d(MDS1,MDS2,MDS3))
# with(soil.s,text3d(x=MDS1,y=MDS2,z=MDS3,text=Row.Names))


# s3d <- scatterplot3d(soil.s)
# text(soil.s,labels=row.names(soil.s))
