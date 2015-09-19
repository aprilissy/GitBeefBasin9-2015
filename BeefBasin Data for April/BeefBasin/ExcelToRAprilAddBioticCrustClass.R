


#Note: the package that this depends on only works with the 32 bit version of R!!!!!!!! 




# install.packages("rJava", dependencies = T)
# install.packages("XLConnect", dependencies = T)
# install.packages("XLConnectJars", dependencies = T)

setwd("F:BeefBasin Data For April/BeefBasin")
library(XLConnect)

#Load workbook
 wb <- loadWorkbook("./BeefBasin_allPedons_edit.xlsx")
 
#PedonIDs
 pedonID <- readWorksheet(wb, sheet = c(2:78), startRow = 2, startCol = 4, endRow = 2, endCol = 4, header = FALSE)
 
#Horizon level Data
	#Read horizon data from each worksheet. If you don't know how many tabs/worksheets you have, enter in a large number and this will throw an error saying that you only have 1:x worksheets. 
	 hzdat <- readWorksheet(wb, sheet = c(2:78), startRow = 15, startCol = 2, endRow = 25, endCol = 17, header = T)
 
	#Remove all rows that are empty (NA)
	 nafunc <- function(x) x[rowSums(is.na(x)) != 15,] #15 works, though there are 16 columns. Change if needed. 
	  hzdat1 <- lapply(hzdat, nafunc)
	 
	#Replicate the pedonID the number of horizons in each pedon
	 pedN <- list()
	  for (i in 1:length(hzdat1)){
	  pedN[[i]] <- data.frame(unlist(rep(pedonID[[i]], times = nrow(hzdat1[[i]]))))
	  }
	  
	 #Change column names
	  pedN <- lapply(pedN, setNames, 'PedonID')
 
    #Join pedonID's and horizon data, then collapse into one dataframe, and write to .csv
      pedons <- mapply(cbind, pedN, hzdat1, SIMPLIFY = FALSE)
       allPedons <- do.call("rbind", pedons)
	    
		write.csv(allPedons, file = "./formattedR/allPedons.csv", row.names = F)

		
		
#**************		
#Site data. Join with pedonId's, reads as list so collapse to dataframe, remove emplty columns, rename some columns
 siteD <- readWorksheet(wb, sheet = c(2:78), startRow = 6, startCol = 3, endRow = 7, endCol = 10, header = TRUE)	 
  siteD2 <- do.call("rbind", siteD)
    siteD3 <- siteD2[,-c(2,7)] 
    names(siteD3)[3:6] <- c('Slope', 'SlopeShape', 'ParentMaterial1', 'BedrockGeology1') #Match April's data

	#Infiltration restriction layer  
	 irl <- readWorksheet(wb, sheet = c(2:78), startRow = 14, startCol = 18, endRow = 15, endCol = 20, header = T) 
	 irl1 <- do.call('rbind', irl)

    #Carbonate stages
	 CS <- readWorksheet(wb, sheet = c(2:78), startRow = 6, startCol = 18, endRow = 7, endCol = 18, header = T) 
	 CS1 <- do.call('rbind', CS)
	  names(CS1) <- 'CarbonateStage'

    #Biotic Crust Class
  BC <- readWorksheet(wb, sheet = c(2:78), startRow = 9, startCol = 5, endRow = 10, endCol = 5, header = T) 
  BC1 <- do.call('rbind', BC)
    names(BC1) <- 'BioticCrustClass'


 #Join dataframes,assign pedonID's, and write to .csv
  AllSite <- cbind(siteD3, irl1, CS1,BC1)
 	  for (i in 1:length(pedonID)){
	   AllSite$pedonID[i] <- pedonID[[i]][1,1]
	   }
	   
     write.csv(AllSite, file = "./formattedR/Site_Data.csv", row.names = F)
 

	   
#GPS coordinates: Read, plot, and write to .csv   
	locInfo <- readWorksheetFromFile(file = "./Field_DataPoints_2013_2014_edit.xlsx", sheet = 2, startRow = 1, startCol = 1, endRow = 78, endCol = 7, header = TRUE)

    #plot(Northing~Easting, data = locInfo, pch = 19)
	 write.csv(locInfo, file = "./formattedR/locInfo.csv", row.names = F)
	 
	 





	 

