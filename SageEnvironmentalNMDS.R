# Do oridnation with soil properties, then put on the veg vecotrs to see
# how they relate to the properties.

# veg vectors:
# 1   Sage density (live+dead)
# 2   Sage density (live)
# 3   Proportion Sage individuals alive (talk to Susan about how to deal with sage absences)
# 4   Proportion Sage cover alive
# 5   Sage relative cover (live+dead)
# 6   Sage relative cover (live)
# 7   Perrenial Grass ralative cover

library(plyr)

#read in shrub density detail data
class <- read.csv('F:/ShrubDensity/HeightClass/PlantDenDetail 8-21.csv')


# 1
# Sage density (live+dead)

