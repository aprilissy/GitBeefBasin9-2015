# http://stackoverflow.com/questions/12436902/overlaying-clustering-results-on-an-ordination

require(vegan)
data <- read.csv("F:/LPI/Output/USGSLPIPercentCover.csv",header=TRUE, row.names=1)

dij <- vegdist(data) ## bray curtis dissimilarity
clu <- hclust(dij, method = "average")
grp <- cutree(clu, 3)

set.seed(2) ## setting a seed to make this reproducible
ord <- metaMDS(data)

col <- c("red2", "green4", "mediumblue")
col[grp]


plot(ord, type = "n", display = "sites")
points(ord, col = col[grp], bg = col[grp], pch = 21)
legend("topright", legend = paste("Cluster", 1:3),
       col = col, pt.bg = col, bty = "n", pch = 21)


ordihull(ord, groups = grp, display = "sites")
