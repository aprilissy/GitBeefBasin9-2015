#Soil-Veg relationships

#install.packages('plotKML', dependencies = TRUE) #, repos="http://r-forge.r-project.org") #install plotKML first
#install.packages(c('aqp', 'GSIF'), dependencies = TRUE) #, repos="http://r-forge.r-project.org")
#install.packages('RColorBrewer', dependencies = TRUE)
#install.packages('ggmap', dependencies = T)
#install.packages("rggobi", dep=T)
#install.packages("randomForest", dep = T)
# if installing aqp and gsif from RForge then also install these packages 
#install.packages('plyr','reshape','scales','Hmisc','stringr','plotrix','dismo','RSAGA','spacetime','FNN','classInt','pixmap','colorRamps') dependencies = TRUE)


setwd("F:/BeefBasin Data for April/BeefBasin")
library(rgdal)
library(ggmap)
library(ggplot2)
library(aqp)
library(plyr)
library(reshape2)

#1. USGS (cLHS) pedon data
{
 #1.a Horizon data. Remove useless columns and make names the same as April's data
  usgsH <- read.csv("./formattedR/allPedons.csv", header = TRUE)
   usgsH1 <- usgsH[,-c(2,3)]
   names(usgsH1)[c(15,17)] <- c('SandPercent', 'ClayPercent')
   usgsH2 <- usgsH1[,-c(11,13)] #Remove RF_knd, RF_sz (not needed)
   usgsH2 <- usgsH2[c(1:10,12:17,11)] #reshuffle column names for easy joining with April's data
   

 #1.b Rosetta data, give meaningful names, calculate Available Water Holding Capacity (AWC)
  usgsR <- read.csv("./Rosetta/cLHS_Rosetta_AWC.csv", header = TRUE)
	usgsR[,3] <- round(usgsR[,3], 3) #round to 3 decimal places
    	
    #Join horizon data and AWC  
     usgsH3 <- cbind(usgsH2, usgsR$AWHC)
     names(usgsH3)[18] <- 'AWC'	

	  
 #1.c Site data. Keep CarbonateState, ExcavationDepth, IRLDepth, IRL150
  usgsS <- read.csv("./formattedR/Site_Data.csv", header = TRUE)
   usgsS1 <- usgsS[,c(11,10,7:9)]
   
 #1.d Location data
  usgsL <- read.csv("./formattedR/locInfo.csv", header = TRUE)
   names(usgsL) [1:3] <- c('PedonID', 'Latitude', 'Longitude') #Make names consistent with April's data
   usgsL <- usgsL[ , -which(names(usgsL) %in% 'Date')] #Remove date field
} 
 
 
 
#2. April pedon data
{
 #2.a horizon data
  aH <- read.csv("./SoilData_cwb.csv", header = TRUE)
	aH1 <- aH[,c(1:21)]#remove site data
	names(aH1)[c(1,21)] <- c('PedonID', 'Effer') #make names match usgs data
	#April entered rock fragment percentages by size class. To get total RF sum over all size classes
	aH1$RF_perc <- apply(aH1[,c(12:16)], 1, FUN = sum, na.rm = T)
     aH2 <- aH1[,-c(12:16)]
	
 #2.b Rosetta data, give meaningful names, calculate Available Water Holding Capacity (AWC)
  aR <- read.csv("./Rosetta/April_Rosetta_AWC.csv", header = TRUE)
   aR[,3] <- round(aR[,3], 3) #round to 3 decimal places
    
 
	#Join horizon data and AWC  
     aH3 <- cbind(aH2, aR$AWHC)
     names(aH3)[c(2,3,18)] <- c('Top','Bottom','AWC')

	 
 #2.b April site data
  aS <- read.csv("./SiteData.csv", header = TRUE)
   aS1 <- aS[,c(1,12,14:16)] #Keep CarbonateState, ExcavationDepth, IRLDepth, IRL150
   names(aS1)[1] <- 'PedonID'
  
  
 #2.c April Location data
	aL <-read.csv("./locInfo.csv", header = TRUE)
	 names(aL)[c(1,4,5)] <- c('PedonID','Northing', 'Easting')
}


	
#3. Vegetation data
 Cveg <- read.csv("./VegetationDataCompiled_edit.csv")
  Cveg <- Cveg[,-1] #Remove first column, unneeded 
{ 
#Metadata of column names for veg data
# ARTR2_L_Total	    Count all live Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_D_Total	    Count all dead Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_ALL_Total	    Count total (all live and dead) Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_L_Mature	    Count all live mature Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_D_Mature	    Count all dead mature Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_ALL_Mature	Count total (all live and dead) mature Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_L_GT050	    Count all live > 50 cm Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_D_GT050	    Count all dead > 50 cm Artemisia tridentata in 3 60 m2 belt transecs
# ARTR2_ALL_GT050	    Count total (all live and dead) > 50 cm Artemisia tridentata in 3 60 m2 belt transecs
# ATCA2_L_Total	    Count all live Atriplex canescens (4-wing) in 3 60 m2 belt transecs
# ATCA2_D_Total	    Count all dead Atriplex canescens (4-wing) in 3 60 m2 belt transecs
# ATCA2_ALL_Total	    Count total (all live and dead) Atriplex canescens (4-wing) in 3 60 m2 belt transecs
# EPVI_L_Total	    Count all live Ephedra in 3 60 m2 belt transecs
# EPVI_D_Total	    Count all dead Ephedra in 3 60 m2 belt transecs
# EPVI_ALL_Total	    Count total (all live and dead) Ephedra in 3 60 m2 belt transecs
# KRLA2_L_Total	    Count all live Krascheninnikovia lanata (winter fat) in 3 60 m2 belt transecs
# KRLA2_D_total	    Count all dead Krascheninnikovia lanata (winter fat) in 3 60 m2 belt transecs
# KRLA2_ALL_total	    Count total (all live and dead) Krascheninnikovia lanata (winter fat) in 3 60 m2 belt transecs
# JUOS_PIED_Total	    Count all live PJ  in 3 60 m2 belt transecs
# LPI_ARTR_D_DP	    Foliar cover dead/decadent Artemisia tridentata (fraction of hits)
# LPI_ARTR_Alive	    Foliar alive (not dead or decadent) Artemisia tridentata (fraction of hits)
# LPI_ARTR_all	    Foliar cover all (live + dead) Artemisia tridentata (fraction of hits)
# Bare_Soil	        Cover of bare soil (no canopy, no ground cover)(fraction of hits)
# PerGrass	        Foliar cover of perennial grasses (fraction of hits)
# Total_Foliar	    Total foliar cover (fraction of hits)
# Total_Litter	    Total litter cover (fraction of hits)
}




#4. Combine datasets
 #4.a Horizon data - total 176 pedons.
  cHori <- rbind(usgsH3,aH3) #Combined USGS and April's pedons - need to export these as KML for coolness.
  
  #In order to investigate relationships between pedon data and veg, I must have one value per variable per pedon location. 
   #Max Clay
    MaxClay <- ddply(cHori, 'PedonID', summarize, MaxClay = max(ClayPercent, na.rm = T))

   #Max Rock fragment content	
	MaxRF <- ddply(cHori, 'PedonID', summarize, MaxRF = max(RF_perc, na.rm = T))
	
   #AWC
    TotalAWC <- ddply(cHori, 'PedonID', summarize, TotalAWC = sum(AWC, na.rm = T))
	MaxAWC <- ddply(cHori, 'PedonID', summarize, MaxAWC = max(AWC, na.rm = T))
	
	
	
   
 
   #Now calculate depth weighted averages of each continuous variable, then append these to the other variables. 
  	#Convert to SoilProfileCollection
	 depths(cHori) <- PedonID ~ Top + Bottom

	# within each profile, compute weighted means, over the intervals: 0-25,0-50,0-100, removing NA if present 
	 d25 <- slab(cHori, PedonID ~ AWC, slab.structure = c(0,25), slab.fun = mean, na.rm=TRUE)
	 d50 <- slab(cHori, PedonID ~ AWC, slab.structure = c(0,50), slab.fun = mean, na.rm=TRUE)
	 d100 <- slab(cHori, PedonID ~ AWC, slab.structure = c(0,100), slab.fun = mean, na.rm=TRUE)
	
	# reshape to wide format, remove unneeded variables and rename. 
	 AWC25 <- dcast(d25, PedonID + top + bottom ~ variable, value.var = 'value')
	  AWC25 <- AWC25[,-c(2,3)]
	  names(AWC25)[2] <- 'AWC25'
	 
	 AWC50 <- dcast(d50, PedonID + top + bottom ~ variable, value.var = 'value')
	  AWC50 <- AWC50[,-c(2,3)]
	  names(AWC50)[2] <- 'AWC50'
	  
	 AWC100 <- dcast(d100, PedonID + top + bottom ~ variable, value.var = 'value')
	  AWC100 <- AWC100[,-c(2,3)]
	  names(AWC100)[2] <- 'AWC100'
 
 
 
  #4.b Site data
   cSite <- rbind(usgsS1, aS1) #Combined USGS and April's site data
    
   #Change IRLDepth 150+ to 150 for plotting purposes (makes everything numeric). 
    cSite$IRLDepth <- ifelse(cSite$IRLDepth == '150+',150,as.numeric(as.character(cSite$IRLDepth))) 
   
   #Was there an infiltration restricting layer within 100 or 50 cm? Some of these are NA because my IRLDepth was NA if the excavation depth did not reach 150cm. However, if excavation depth was > 100 or 50 cm, then I can change NA values to 'N' - I would have to do this mannually. 
    cSite$IRL100 <- ifelse(cSite$IRLDepth >  100, 'N', 'Y') 	 
    cSite$IRL50 <- ifelse(cSite$IRLDepth >  50, 'N', 'Y') 
   
   
  #4.c Location data
{ 
 cLoc <- rbind(usgsL, aL) #Combined USGS and April's GPS location info
  
  #Plot boundary and points on a Google map, because it's cool.
   #study area boundary
    # BB_bound <- readOGR("./Spatial_Data/StudyArea", "NCSS_StudyArea5Feb2013") 
    # BB_bound2 <- spTransform(BB_bound, CRS("+proj=longlat +datum=WGS84")) #Convert from UTM to lat/long for plotting.  
     # dfBound <- fortify(BB_bound2) #Convert to dataframe for plotting
 
 #Get Google map 
  # BBmap <- get_map(location = c(lon = -109.9, lat = 37.99), zoom = 12,maptype = "terrain", scale = 2)
  
  #Plot polygon and points on map
   # ggmap(BBmap) +
    # geom_polygon(aes(x=long, y=lat, group=group), data = dfBound, fill = NA, colour = 'black', size = 0.75) + 
    # geom_point(aes(x = Longitude, y = Latitude), data = cLoc, alpha = 0.7)
}

  
#4d. Merge all Site data into one dataset for soil-veg relationships
  s1 <- join(cLoc, cSite, by = 'PedonID', type = 'inner')
  s2 <- join(s1, MaxClay, by = 'PedonID', type = 'inner')
  s3 <- join(s2, MaxRF, by = 'PedonID', type = 'inner')
  s4 <- join(s3, MaxAWC, by = 'PedonID', type = 'inner')
  s5 <- join(s4, TotalAWC, by = 'PedonID', type = 'inner')
  s6 <- join(s5, AWC25, by = 'PedonID', type = 'inner')
  s7 <- join(s6, AWC50, by = 'PedonID', type = 'inner')
  s8 <- join(s7, AWC100, by = 'PedonID', type = 'inner')
  

 #Join soil and veg data, then write to .csv
  SV <- join(s8, Cveg, by = 'PedonID', type = 'inner')
   #Make sure that categorical data is a factor. 
   SV$CarbonateStage <- as.factor(SV$CarbonateStage)
   SV$IRL100 <- as.factor(SV$IRL100)
   SV$IRL50 <- as.factor(SV$IRL50)
    write.csv(SV, "./CombinedSoilVegData.csv", row.names = FALSE)
 
   
   

	
	
	

	
	
#TO DO when I get back from SSSA:
 
 
#3. AQP plots by veg gradients and by Jamin's ecosite classification (put in site data). I envision plotting individual pedons along ARTR_L_Total (etc.) gradients (e.g. http://aqp.r-forge.r-project.org/aqp_profile_plot_example_sjer.png) I also envision slab plots by ecosite such as: http://casoilresource.lawr.ucdavis.edu/drupal/files/images/ca630_morph_vs_depth_by_bedrock_kind.img_assist_custom.png
 #and 
 #slides 14 and 15 http://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_053209.pdf. 
	


#if the profile ID occurs in the "site" table, you can splice it in like this. 
	# splice in site data with a left join on "id"
site(x) <- site.data
#Note that you can do this with any data that shares an ID with the site data.
	site(cHori) <- SV
	
	


	
	 plot(s[1:10],color = "SandPercent")
	 
	


		
 #4. Some sort of numerical clustering? See notes from 30 Oct. 2014
 #Classify sites into shrub, tree, bare, grass... According to Mike: "I would view the LPI sage cover or belt transect density  “alive” as where sage as persisted, “dead/decadent” as where it has come but not persisted, and “all” as everywhere it has ever been. The Ephedra, 4-wing, and winter fat should all be indicators of the grassland site (according to Jamin)". We could split veg data into classes shrub(persisted), shrub(transient), shrub(ever been), grassland, tree, bare
 
 #Figure out how to make kml files for coolness. 
 
 
 
 
 
 
 
 
 
 
 #5.1 Graphical soil-veg EDA
#Correlations
{  
    cors <- cor(y = SV[,c(9,13:19)],x = SV[,c(20:45)], use = 'complete.obs') #For some reason I get different correlations when I use the matrix vs. vector notation, so I have to calculate correlation for each variable individually. 
	 #write.csv(cors, "./correlations.csv")
	
	cor(y=SV$IRLDepth, x=SV$ARTR2_Total, use = "complete.obs")    #
	cor(y=SV$IRLDepth, x=SV$ARTR2_D_Total, use = "complete.obs")  #
	cor(y=SV$IRLDepth, x=SV$ARTR2_Mature, use = "complete.obs")   #
	cor(y=SV$IRLDepth, x=SV$ARTR2_D_Mature, use = "complete.obs") #
	cor(y=SV$IRLDepth, x=SV$ARTR2_GT050, use = "complete.obs")    #
	cor(y=SV$IRLDepth, x=SV$ARTR2_D_GT050, use = "complete.obs")  #
	cor(y=SV$IRLDepth, x=SV$ATCA2_Total, use = "complete.obs")    #
	cor(y=SV$IRLDepth, x=SV$ATCA2_D_Total, use = "complete.obs")  #
	cor(y=SV$IRLDepth, x=SV$EPVI_Total, use = "complete.obs")     #
	cor(y=SV$IRLDepth, x=SV$KRLA2_Total, use = "complete.obs")    #
	cor(y=SV$IRLDepth, x=SV$KRLA2_D_total, use = "complete.obs")  #
	cor(y=SV$IRLDepth, x=SV$JUOS_PIED_Total, use = "complete.obs")# -0.326235
	cor(y=SV$IRLDepth, x=SV$LPI_ARTR_D_DP, use = "complete.obs")  #
	cor(y=SV$IRLDepth, x=SV$LPI_ARTR_Alive, use = "complete.obs") #
	cor(y=SV$IRLDepth, x=SV$LPI_ARTR_all, use = "complete.obs")   #
	cor(y=SV$IRLDepth, x=SV$Bare_Soil, use = "complete.obs")      #
	cor(y=SV$IRLDepth, x=SV$PerGrass, use = "complete.obs")       #0.3228633
	cor(y=SV$IRLDepth, x=SV$Total_Foliar, use = "complete.obs")   #0.362733, all others < 0.2
	cor(y=SV$IRLDepth, x=SV$Total_Litter, use = "complete.obs")   #
	
	
	cor(y=SV$MaxClay, x=SV$ARTR2_Total, use = "complete.obs")      #
	cor(y=SV$MaxClay, x=SV$ARTR2_D_Total, use = "complete.obs")    #
	cor(y=SV$MaxClay, x=SV$ARTR2_Mature, use = "complete.obs")     #
	cor(y=SV$MaxClay, x=SV$ARTR2_D_Mature, use = "complete.obs")   #
	cor(y=SV$MaxClay, x=SV$ARTR2_GT050, use = "complete.obs")      #
	cor(y=SV$MaxClay, x=SV$ARTR2_D_GT050, use = "complete.obs")    #
	cor(y=SV$MaxClay, x=SV$ATCA2_Total, use = "complete.obs")      #
	cor(y=SV$MaxClay, x=SV$ATCA2_D_Total, use = "complete.obs")    #
	cor(y=SV$MaxClay, x=SV$EPVI_Total, use = "complete.obs")       #
	cor(y=SV$MaxClay, x=SV$KRLA2_Total, use = "complete.obs")      #
	cor(y=SV$MaxClay, x=SV$KRLA2_D_total, use = "complete.obs")    #
	cor(y=SV$MaxClay, x=SV$JUOS_PIED_Total, use = "complete.obs")  #
	cor(y=SV$MaxClay, x=SV$LPI_ARTR_D_DP, use = "complete.obs")    #
	cor(y=SV$MaxClay, x=SV$LPI_ARTR_Alive, use = "complete.obs")   #
	cor(y=SV$MaxClay, x=SV$LPI_ARTR_all, use = "complete.obs")     #
	cor(y=SV$MaxClay, x=SV$Bare_Soil, use = "complete.obs")        # 0.22, all others < 0.12
	cor(y=SV$MaxClay, x=SV$PerGrass, use = "complete.obs")         #
	cor(y=SV$MaxClay, x=SV$Total_Foliar, use = "complete.obs")     #
	cor(y=SV$MaxClay, x=SV$Total_Litter, use = "complete.obs")     #
	
	cor(y=SV$MaxRF, x=SV$ARTR2_Total, use = "complete.obs")      #
	cor(y=SV$MaxRF, x=SV$ARTR2_D_Total, use = "complete.obs")    #
	cor(y=SV$MaxRF, x=SV$ARTR2_Mature, use = "complete.obs")     #
	cor(y=SV$MaxRF, x=SV$ARTR2_D_Mature, use = "complete.obs")   #
	cor(y=SV$MaxRF, x=SV$ARTR2_GT050, use = "complete.obs")      #
	cor(y=SV$MaxRF, x=SV$ARTR2_D_GT050, use = "complete.obs")    #
	cor(y=SV$MaxRF, x=SV$ATCA2_Total, use = "complete.obs")      #
	cor(y=SV$MaxRF, x=SV$ATCA2_D_Total, use = "complete.obs")    #
	cor(y=SV$MaxRF, x=SV$EPVI_Total, use = "complete.obs")       #
	cor(y=SV$MaxRF, x=SV$KRLA2_Total, use = "complete.obs")      #
	cor(y=SV$MaxRF, x=SV$KRLA2_D_total, use = "complete.obs")    # 
	cor(y=SV$MaxRF, x=SV$JUOS_PIED_Total, use = "complete.obs")  # 0.3606779
	cor(y=SV$MaxRF, x=SV$LPI_ARTR_D_DP, use = "complete.obs")    #
	cor(y=SV$MaxRF, x=SV$LPI_ARTR_Alive, use = "complete.obs")   #
	cor(y=SV$MaxRF, x=SV$LPI_ARTR_all, use = "complete.obs")     #
	cor(y=SV$MaxRF, x=SV$Bare_Soil, use = "complete.obs")        #  0.2151919
	cor(y=SV$MaxRF, x=SV$PerGrass, use = "complete.obs")         # -0.3242188
	cor(y=SV$MaxRF, x=SV$Total_Foliar, use = "complete.obs")     # -0.3887385, all others < 0.2
	cor(y=SV$MaxRF, x=SV$Total_Litter, use = "complete.obs")     #
	
	cor(y=SV$TotalAWC, x=SV$ARTR2_Total, use = "complete.obs")      #
	cor(y=SV$TotalAWC, x=SV$ARTR2_D_Total, use = "complete.obs")    # 0.2082827
	cor(y=SV$TotalAWC, x=SV$ARTR2_Mature, use = "complete.obs")     #
	cor(y=SV$TotalAWC, x=SV$ARTR2_D_Mature, use = "complete.obs")   # 0.2075814
	cor(y=SV$TotalAWC, x=SV$ARTR2_GT050, use = "complete.obs")      #
	cor(y=SV$TotalAWC, x=SV$ARTR2_D_GT050, use = "complete.obs")    #
	cor(y=SV$TotalAWC, x=SV$ATCA2_Total, use = "complete.obs")      #
	cor(y=SV$TotalAWC, x=SV$ATCA2_D_Total, use = "complete.obs")    #
	cor(y=SV$TotalAWC, x=SV$EPVI_Total, use = "complete.obs")       #
	cor(y=SV$TotalAWC, x=SV$KRLA2_Total, use = "complete.obs")      #
	cor(y=SV$TotalAWC, x=SV$KRLA2_D_total, use = "complete.obs")    #
	cor(y=SV$TotalAWC, x=SV$JUOS_PIED_Total, use = "complete.obs")  # -0.285565
	cor(y=SV$TotalAWC, x=SV$LPI_ARTR_D_DP, use = "complete.obs")    #
	cor(y=SV$TotalAWC, x=SV$LPI_ARTR_Alive, use = "complete.obs")   #
	cor(y=SV$TotalAWC, x=SV$LPI_ARTR_all, use = "complete.obs")     #
	cor(y=SV$TotalAWC, x=SV$Bare_Soil, use = "complete.obs")        # 
	cor(y=SV$TotalAWC, x=SV$PerGrass, use = "complete.obs")         # 0.3788754
	cor(y=SV$TotalAWC, x=SV$Total_Foliar, use = "complete.obs")     # 0.3507901, all others < 0.2
	cor(y=SV$TotalAWC, x=SV$Total_Litter, use = "complete.obs")     #
	
	cor(y=SV$MaxAWC, x=SV$ARTR2_Total, use = "complete.obs")      #
	cor(y=SV$MaxAWC, x=SV$ARTR2_D_Total, use = "complete.obs")    # 0.2965547
	cor(y=SV$MaxAWC, x=SV$ARTR2_Mature, use = "complete.obs")     # 
	cor(y=SV$MaxAWC, x=SV$ARTR2_D_Mature, use = "complete.obs")   # 0.2929614
	cor(y=SV$MaxAWC, x=SV$ARTR2_GT050, use = "complete.obs")      #
	cor(y=SV$MaxAWC, x=SV$ARTR2_D_GT050, use = "complete.obs")    #
	cor(y=SV$MaxAWC, x=SV$ATCA2_Total, use = "complete.obs")      #
	cor(y=SV$MaxAWC, x=SV$ATCA2_D_Total, use = "complete.obs")    # 
	cor(y=SV$MaxAWC, x=SV$EPVI_Total, use = "complete.obs")       #
	cor(y=SV$MaxAWC, x=SV$KRLA2_Total, use = "complete.obs")      #
	cor(y=SV$MaxAWC, x=SV$KRLA2_D_total, use = "complete.obs")    #
	cor(y=SV$MaxAWC, x=SV$JUOS_PIED_Total, use = "complete.obs")  # -0.4528403
	cor(y=SV$MaxAWC, x=SV$LPI_ARTR_D_DP, use = "complete.obs")    # 0.2921304
	cor(y=SV$MaxAWC, x=SV$LPI_ARTR_Alive, use = "complete.obs")   #
	cor(y=SV$MaxAWC, x=SV$LPI_ARTR_all, use = "complete.obs")     # 0.279483
	cor(y=SV$MaxAWC, x=SV$Bare_Soil, use = "complete.obs")        # 
	cor(y=SV$MaxAWC, x=SV$PerGrass, use = "complete.obs")         # 0.5945984
	cor(y=SV$MaxAWC, x=SV$Total_Foliar, use = "complete.obs")     # 0.5351836
	cor(y=SV$MaxAWC, x=SV$Total_Litter, use = "complete.obs")     #  -0.3396441, all others < 0.2
}	
	
	
#Simple Bivariate plots of those with correlations > 0.2	
{	
 #Depth to infiltration restricting layer (Cr or R). Correlations plotted on graphs
 png('.plots/IRLDepth_veg.png', width = 8.5, height = 11, units = 'in', res = 150)
	par(mfcol = c(3,1), oma = c(1,1,2,1))
	
	#plot(ARTR2_Total      ~IRLDepth, data = SV, pch = 19, col = 'blue')
     #text(x=25,y=0.95*max(SV$ARTR2_Total, na.rm = TRUE), paste('cor =',round(cor(SV$IRLDepth, SV$ARTR2_Total, use = "complete.obs"),3)))
	#plot(ARTR2_Mature    ~IRLDepth, data = SV, pch = 19, col = 'blue')
	 #text(x=25,y=0.95*max(SV$ARTR2_Mature, na.rm = TRUE), paste('cor =',round(cor(SV$IRLDepth, SV$ARTR2_Mature, use = "complete.obs"),3)))
	plot(JUOS_PIED_Total  ~IRLDepth, data = SV, pch = 19, col = 'blue')
	 text(x=25,y=0.95*max(SV$JUOS_PIED_Total, na.rm = TRUE), paste('cor =',round(cor(SV$IRLDepth, SV$JUOS_PIED_Total, use = "complete.obs"),3)))
	plot(PerGrass         ~IRLDepth, data = SV, pch = 19, col = 'blue') 
	 text(x=25,y=0.95*max(SV$PerGrass, na.rm = TRUE), paste('cor =',round(cor(SV$IRLDepth, SV$PerGrass, use = "complete.obs"),3)))
	plot(Total_Foliar     ~IRLDepth, data = SV, pch = 19, col = 'blue')
	 text(x=25,y=0.95*max(SV$Total_Foliar, na.rm = TRUE), paste('cor =',round(cor(SV$IRLDepth, SV$Total_Foliar, use = "complete.obs"),3)))
	 
	 mtext("Depth to R or Cr - infiltration restricting", outer = T, cex = 1.5)
 dev.off()	
	
 #Max clay
 png('.plots/MaxClay_veg.png', width = 8.5, height = 11, units = 'in', res = 150) 
	plot(Bare_Soil  ~ MaxClay, data = SV, pch = 19, col = 'blue', main = 'Maximum Clay')
	 text(x=5,y=0.95*max(SV$Bare_Soil ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxClay, x=SV$Bare_Soil, use = "complete.obs"),3)))      
 dev.off()
 
 #Max RF
 png('./plots/MaxRF_veg.png', width = 8.5, height = 11, units = 'in', res = 150)
 par(mfcol = c(2,2), oma = c(1,1,2,1))
 
    #plot(KRLA2_D_total  ~ MaxRF, data = SV, pch = 19, col = 'blue')
     #text(x=10,y=0.95*max(SV$KRLA2_D_total ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxRF, x=SV$KRLA2_D_total, use = "complete.obs"),3)))	
	plot(JUOS_PIED_Total~ MaxRF, data = SV, pch = 19, col = 'blue') 
	 text(x=10,y=0.95*max(SV$JUOS_PIED_Total ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxRF, x=SV$JUOS_PIED_Total, use = "complete.obs"),3)))
	plot(Bare_Soil      ~ MaxRF, data = SV, pch = 19, col = 'blue') 
	 text(x=10,y=0.95*max(SV$Bare_Soil ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxRF, x=SV$Bare_Soil, use = "complete.obs"),3))) 
	plot(PerGrass       ~ MaxRF, data = SV, pch = 19, col = 'blue')
	 text(x=10,y=0.95*max(SV$PerGrass ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxRF, x=SV$PerGrass, use = "complete.obs"),3)))
	plot(Total_Foliar   ~ MaxRF, data = SV, pch = 19, col = 'blue')    
     text(x=10,y=0.95*max(SV$Total_Foliar ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxRF, x=SV$Total_Foliar, use = "complete.obs"),3)))
	    
 mtext("Maximum Rock Fragments", outer = T, cex = 1.5) 
 dev.off()	
	
 #Total AWC
 png('./Soil-VegRelationships/plots/TotalAWC_veg.png', width = 8.5, height = 11, units = 'in', res = 150)
 par(mfrow = c(2,3), oma = c(1,1,2,1))
 
  plot(ARTR2_D_Total ~TotalAWC, data = SV, pch = 19, col = 'blue')
   text(x=1,y=0.95*max(SV$ARTR2_D_Total ,na.rm = TRUE), paste('cor =',round(cor(y=SV$TotalAWC, x=SV$ARTR2_D_Total, use = "complete.obs"),3)))
  plot(ARTR2_D_Mature ~TotalAWC, data = SV, pch = 19, col = 'blue')
   text(x=1,y=0.95*max(SV$ARTR2_D_Mature ,na.rm = TRUE), paste('cor =',round(cor(y=SV$TotalAWC, x=SV$ARTR2_D_Mature, use = "complete.obs"),3)))    
  plot(JUOS_PIED_Total ~TotalAWC, data = SV, pch = 19, col = 'blue')
   text(x=1,y=0.95*max(SV$JUOS_PIED_Total ,na.rm = TRUE), paste('cor =',round(cor(y=SV$TotalAWC, x=SV$JUOS_PIED_Total, use = "complete.obs"),3)))
  plot(PerGrass        ~TotalAWC, data = SV, pch = 19, col = 'blue')
   text(x=1,y=0.95*max(SV$PerGrass ,na.rm = TRUE), paste('cor =',round(cor(y=SV$TotalAWC, x=SV$PerGrass, use = "complete.obs"),3)))     
  plot(Total_Foliar    ~TotalAWC, data = SV, pch = 19, col = 'blue')
   text(x=1,y=0.95*max(SV$Total_Foliar ,na.rm = TRUE), paste('cor =',round(cor(y=SV$TotalAWC, x=SV$Total_Foliar, use = "complete.obs"),3))) 
 
 mtext("Total AWC", outer = T, cex = 1.5)
 dev.off()	  	

 #Max AWC
 png('./Soil-VegRelationships/plots/MaxAWC_veg.png', width = 8.5, height = 11, units = 'in', res = 150)
  par(mfcol = c(4,2), oma = c(1,1,2,1)) 
  
    plot(ARTR2_D_Total   ~MaxAWC, data = SV, pch = 19, col = 'blue') 
     text(x=0.34,y=0.95*max(SV$ARTR2_D_Total ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$ARTR2_D_Total, use = "complete.obs"),3)))	
	plot(ARTR2_D_Mature   ~MaxAWC, data = SV, pch = 19, col = 'blue')
     text(x=0.34,y=0.95*max(SV$ARTR2_D_Mature ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$ARTR2_D_Mature, use = "complete.obs"),3)))    
	plot(JUOS_PIED_Total ~MaxAWC, data = SV, pch = 19, col = 'blue') 
     text(x=0.34,y=0.95*max(SV$JUOS_PIED_Total ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$JUOS_PIED_Total, use = "complete.obs"),3)))
	plot(LPI_ARTR_D_DP   ~MaxAWC, data = SV, pch = 19, col = 'blue')
     text(x=0.34,y=0.95*max(SV$LPI_ARTR_D_DP ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$LPI_ARTR_D_DP, use = "complete.obs"),3))) 
	plot(LPI_ARTR_all    ~MaxAWC, data = SV, pch = 19, col = 'blue') 
      text(x=0.34,y=0.95*max(SV$LPI_ARTR_all ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$LPI_ARTR_all, use = "complete.obs"),3)))    
	plot(PerGrass        ~MaxAWC, data = SV, pch = 19, col = 'blue')
      text(x=0.34,y=0.95*max(SV$PerGrass ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$PerGrass, use = "complete.obs"),3)))	
	plot(Total_Foliar    ~MaxAWC, data = SV, pch = 19, col = 'blue')
     text(x=0.34,y=0.95*max(SV$Total_Foliar ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$Total_Foliar, use = "complete.obs"),3)))	
	plot(Total_Litter    ~MaxAWC, data = SV, pch = 19, col = 'blue')
	 text(x=0.34,y=0.95*max(SV$Total_Litter ,na.rm = TRUE), paste('cor =',round(cor(y=SV$MaxAWC, x=SV$Total_Litter, use = "complete.obs"),3)))
 
  mtext("Maximum AWC", outer = T, cex = 1.5)
  dev.off()
 }
 
#Boxplots of Depth to Cr or R 
{
#Save manually
 par(mfcol=c(3,2), oma=c(1,1,2,1))

  boxplot(ARTR2_Total     ~ IRL150, data = SV, ylab = 'ARTR2_Total') 
  boxplot(ARTR2_D_Total   ~ IRL150, data = SV, ylab = 'ARTR2_D_Total')
  boxplot(ARTR2_Mature    ~ IRL150, data = SV, ylab = 'ARTR2_Mature')
  boxplot(ARTR2_D_Mature  ~ IRL150, data = SV, ylab = 'ARTR2_D_Mature')
  boxplot(ARTR2_GT050     ~ IRL150, data = SV, ylab = 'ARTR2_GT050')
  boxplot(ARTR2_D_GT050   ~ IRL150, data = SV, ylab = 'ARTR2_D_GT050')
  boxplot(LPI_ARTR_D_DP   ~ IRL150, data = SV, ylab = 'LPI_ARTR_D_DP')
  boxplot(LPI_ARTR_Alive  ~ IRL150, data = SV, ylab = 'LPI_ARTR_Alive')
  boxplot(LPI_ARTR_all    ~ IRL150, data = SV, ylab = 'LPI_ARTR_all')
  boxplot(Total_Foliar    ~ IRL150, data = SV, ylab = 'Total_Foliar')
  boxplot(ATCA2_Total     ~ IRL150, data = SV, ylab = 'ATCA2_Total')
  boxplot(ATCA2_D_Total   ~ IRL150, data = SV, ylab = 'ATCA2_D_Total')
  boxplot(EPVI_Total      ~ IRL150, data = SV, ylab = 'EPVI_Total')
  boxplot(EPVI_D_Total    ~ IRL150, data = SV, ylab = 'EPVI_D_Total')
  boxplot(KRLA2_Total     ~ IRL150, data = SV, ylab = 'KRLA2_Total')
  boxplot(KRLA2_D_total   ~ IRL150, data = SV, ylab = 'KRLA2_D_total')
  boxplot(JUOS_PIED_Total ~ IRL150, data = SV, ylab = 'JUOS_PIED_Total')
  boxplot(Bare_Soil       ~ IRL150, data = SV, ylab = 'Bare_Soil')
  boxplot(PerGrass        ~ IRL150, data = SV, ylab = 'PerGrass')
  boxplot(Total_Foliar    ~ IRL150, data = SV, ylab = 'Total_Foliar')
  boxplot(Total_Litter    ~ IRL150, data = SV, ylab = 'Total_Litter')
}	  

#Boxplots of different Depths to IRL
{
#ARTR
png('./Soil-VegRelationships/plots/IRLDepth_ARTR.png', width = 8.5, height = 11, units = 'in', res = 150)
par(mfrow=c(3,3), oma=c(1,1,2,1))
 boxplot(ARTR2_L_Total ~ IRL150, data = SV, ylab = 'ARTR2_L_Total', xlab = 'IRL150') 
 boxplot(ARTR2_L_Total ~ IRL100, data = SV, ylab = 'ARTR2_L_Total', xlab = 'IRL100')
 boxplot(ARTR2_L_Total ~ IRL50, data = SV, ylab = 'ARTR2_L_Total', xlab = 'IRL50')
                       
 boxplot(ARTR2_D_Total ~ IRL150, data = SV, ylab = 'ARTR2_D_Total', xlab = 'IRL150')
 boxplot(ARTR2_D_Total ~ IRL100, data = SV, ylab = 'ARTR2_D_Total', xlab = 'IRL100')
 boxplot(ARTR2_D_Total ~ IRL50, data = SV, ylab = 'ARTR2_D_Total', xlab = 'IRL50')
 
 boxplot(ARTR2_ALL_Total ~ IRL150, data = SV, ylab = 'ARTR2_ALL_Total', xlab = 'IRL150')
 boxplot(ARTR2_ALL_Total ~ IRL100, data = SV, ylab = 'ARTR2_ALL_Total', xlab = 'IRL100')
 boxplot(ARTR2_ALL_Total ~ IRL50, data = SV, ylab = 'ARTR2_ALL_Total', xlab = 'IRL50')
 
mtext("ARTR2 Count", outer = T, cex = 1.5)
dev.off()
 
#ARTR Mature 
png('./Soil-VegRelationships/plots/IRLDepth_ARTR_Mature.png', width = 8.5, height = 11, units = 'in', res = 150)
 par(mfrow=c(3,3), oma=c(1,1,2,1))
  boxplot(ARTR2_L_Mature ~ IRL150, data = SV, ylab = 'ARTR2_L_Mature', xlab = 'IRL150') 
  boxplot(ARTR2_L_Mature ~ IRL100, data = SV, ylab = 'ARTR2_L_Mature', xlab = 'IRL100')
  boxplot(ARTR2_L_Mature ~ IRL50, data = SV, ylab = 'ARTR2_L_Mature', xlab = 'IRL50')
                         
  boxplot(ARTR2_D_Mature ~ IRL150, data = SV, ylab = 'ARTR2_D_Mature', xlab = 'IRL150')
  boxplot(ARTR2_D_Mature ~ IRL100, data = SV, ylab = 'ARTR2_D_Mature', xlab = 'IRL100')
  boxplot(ARTR2_D_Mature ~ IRL50, data = SV, ylab = 'ARTR2_D_Mature', xlab = 'IRL50')
  
  boxplot(ARTER_ALL_Mature ~ IRL150, data = SV, ylab = 'ARTER_ALL_Mature', xlab = 'IRL150')
  boxplot(ARTER_ALL_Mature ~ IRL100, data = SV, ylab = 'ARTER_ALL_Mature', xlab = 'IRL100')
  boxplot(ARTER_ALL_Mature ~ IRL50, data = SV, ylab = 'ARTER_ALL_Mature', xlab = 'IRL50')
 
mtext("ARTR2 Mature Count", outer = T, cex = 1.5)
dev.off() 
 

#ARTR GT050
png('./Soil-VegRelationships/plots/IRLDepth_ARTR_GT050.png', width = 8.5, height = 11, units = 'in', res = 150) 
 par(mfrow=c(3,3), oma=c(1,1,2,1))
  boxplot(ARTR2_L_GT050 ~ IRL150, data = SV, ylab = 'ARTR2_L_GT050', xlab = 'IRL150') 
  boxplot(ARTR2_L_GT050 ~ IRL100, data = SV, ylab = 'ARTR2_L_GT050', xlab = 'IRL100')
  boxplot(ARTR2_L_GT050 ~ IRL50, data = SV, ylab = 'ARTR2_L_GT050', xlab = 'IRL50')
  
  boxplot(ARTR2_D_GT050 ~ IRL150, data = SV, ylab = 'ARTR2_D_GT050', xlab = 'IRL150')
  boxplot(ARTR2_D_GT050 ~ IRL100, data = SV, ylab = 'ARTR2_D_GT050', xlab = 'IRL100')
  boxplot(ARTR2_D_GT050 ~ IRL50, data = SV, ylab = 'ARTR2_D_GT050', xlab = 'IRL50')
  
  boxplot(ARTR2_ALL_GT050 ~ IRL150, data = SV, ylab = 'ARTR2_ALL_GT050', xlab = 'IRL150')
  boxplot(ARTR2_ALL_GT050 ~ IRL100, data = SV, ylab = 'ARTR2_ALL_GT050', xlab = 'IRL100')
  boxplot(ARTR2_ALL_GT050 ~ IRL50, data = SV, ylab = 'ARTR2_ALL_GT050', xlab = 'IRL50')
 
mtext("ARTR2 GT050 Count", outer = T, cex = 1.5)
dev.off()
}
 

#5.2 Model based soil-veg EDA
 library(randomForest)
 
  #Live ARTER- full model 
   artrL1 <- randomForest(ARTR2_L_Total ~ CarbonateStage+IRLDepth+IRL150+IRL100+IRL50+MaxClay+MaxRF+MaxAWC+TotalAWC+AWC25+AWC50+AWC100, data = SV, na.action = na.omit, mtry = 2, ntree = 1000, importance = T)
   artrL
   varImpPlot(artrL)
   
	   #Most important variables, start with all and whittle away until error increases
	   artrL2 <- randomForest(ARTR2_L_Total ~ MaxAWC+AWC100+IRLDepth, data = SV, na.action = na.omit, mtry = 2, ntree = 500, importance = T)   
		artrL2
		plot(artrL2)
		varImpPlot(artrL2)
  
  
  #Dead ARTR - full model
   artrD <- randomForest(ARTR2_D_Total ~ CarbonateStage+IRLDepth+IRL150+IRL100+IRL50+MaxClay+MaxRF+MaxAWC+TotalAWC+AWC25+AWC50+AWC100, data = SV, na.action = na.omit, mtry = 2, ntree = 1000, importance = T)
	artrD
	varImpPlot(artrD)

	  #Most important variables from full model, start with all and whittle away until error increases
	   artrD2 <- randomForest(ARTR2_D_Total ~ IRLDepth+AWC25+IRL150+MaxAWC+IRL50+IRL100, data = SV, na.action = na.omit, mtry = 2, ntree = 1000, importance = T)
		artrD2
		varImpPlot(artrD2)
	
	
		  #Most important variables, from 1st subset, whittle away until error increases. 	
		   artrD3 <- randomForest(ARTR2_D_Total ~ IRLDepth+MaxAWC+IRL50+IRL150+IRL100, data = SV, na.action = na.omit, mtry = 2, ntree = 1000, importance = T)
			artrD3
			varImpPlot(artrD3)
	
	
	#Based on these models, which are terrible for the live stuff, and okay for the dead stuff: 
	 #Live: MaxAWC, AWC100, IRLDepth
	 #Dead: IRLDepth, MaxAWC, IRL50, IRL150, IRL100
 


