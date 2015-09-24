# Soil NMDS with USGS data added to April data
data.env.NS <- read.csv("F:/Soils/SoilEnvironmentalDataNSplain.csv",header=TRUE, row.names=1)
data.env.U <- read.csv("F:/Soils/SoilEnvironmentalDataUSGSApril.csv",header=TRUE, row.names=1)


data.env.NS[is.na(data.env.NS)] <- 0 # replace NA with 0
data.env.U[is.na(data.env.U)] <- 0 # replace NA with 0


sage.NS <- read.csv("F:/SageNMDSvariables/Sage.Env.NSplainApril.csv",header=TRUE,row.names=1)
sage.U <- read.csv("F:/SageNMDSvariables/Sage.Env.USGS.csv",header=TRUE,row.names=1)

sage.NS[is.na(sage.NS)] <- 0 # replace NA with 0
sage.U[is.na(sage.U)] <- 0 # replace NA with 0
