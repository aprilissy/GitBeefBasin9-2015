#Code begun 3 Dec 2014, by Colby Brungard
#Rosetta returns saturated and residual water contents, along with other van Genuchten parameters. 
#AWC is the difference between field capacity and permanent wilting water contents. With the output from Rosetta it is possible to calculate field capacity and permanent wilting points. Then just difference these to get AWC. 
 #setwd("D:/BeefBasin")

#1. Read in Rosetta output, then remove the second header
	cLHSro <- data.frame(read.table('./cLHS_RosettaOutput.txt', header = T, colClasses = "character"))
	cLHSro <- cLHSro[-1,]
	
	Aprilro <- data.frame(read.table('./April_RosettaOutput.txt', header = T, colClasses = "character"))
	Aprilro <- Aprilro[-1,]


#2. Calculate available water holding capacity
#Parameters	
	#x = dataframe of rosetta output
	#h_fc = Field capacity, h = 337 cm  
	#h_pwp = Permanent wilting point, h = 15297 cm. 
		#Conversion from 1500 kPa (pwp) to cm: 1500 kPa * (401.5 inches H2O/100 kPa) * (2.54 cm H2O/1 inches H2O)= 15297 cm. However, using 15000 cm doesn't significantly change AWC (it all still rounds to the same tenth). The same equation for 33 kPa results in 337 cm 	
		
	
ThetaVG <- function(x, h_fc, h_pwp){
 x[x==-9.9] <- NA #Set any null values (-9.9) to NA
 Theta_s <- as.numeric(x$Theta_s)
 Theta_r <- as.numeric(x$Theta_r)
 Alpha <- 10^(as.numeric(x$Alpha)) #Rosetta output is in log10
 N <- 10^(as.numeric(x$N))         #Rosetta output is in log10
 
 Theta_fc  <- Theta_r + ((Theta_s-Theta_r)/((1+((Alpha*h_fc)^N))^(1-(1/N))))  #Field capacity moisture content
 Theta_pwp <- Theta_r + ((Theta_s-Theta_r)/((1+((Alpha*h_pwp)^N))^(1-(1/N)))) #Permanent wilting point moisture content
 AWHC = (Theta_fc-Theta_pwp)*100

 return(cbind(Theta_fc, Theta_pwp, AWHC))
 #This returns Field capacity (cm3/cm3), Permanent wilting point moisture content (cm3/cm3), and Available Water Holding Capacity (%v)
 
}


#Apply the above function to both datasets
 cLHSAWC <- ThetaVG(cLHSro, 337, 15297)
 AprilAWC <- ThetaVG(Aprilro, 337, 15297)

write.csv(cLHSAWC, './cLHS_Rosetta_AWC.csv', row.names = FALSE)
write.csv(AprilAWC, './Rosetta/April_Rosetta_AWC.csv', row.names = FALSE)

