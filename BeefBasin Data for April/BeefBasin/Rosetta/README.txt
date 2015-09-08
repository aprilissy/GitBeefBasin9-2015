#File names

..._AWC.csv - Moisture content at field capacity (Theat_fc, cm3/cm3), moisutre content at permanent wilting point (Theta_pwp, cm3/cm3), and available water holding capacity (AWHC, % volume moisture content). An AWHC of 2 would mean 2% soil water by volume. 

..._RosettaInput.txt - horizon level data formatted for input into Rosetta soil water model. 
..._RosettaOutput.txt - Output from Rosetta soil water model, and input to Rosetta to AWC.r. 

Rosetta ot AWC.r - Code to convert Rosetta Output into available water holding capacity values. This code creates ..._AWC.csv files. 