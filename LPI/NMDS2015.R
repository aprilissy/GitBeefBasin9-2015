#install.packages("vegan")
library(vegan)

#Run an MDS on data set, with columns of variables and rows of samples. The vegan package
#is designed for ecological data, so the metaMDS default settings are set with this in mind. 
mydata <- read.csv("F:/LPI/LPIRelativeCoverCommonInExcel.csv",header=TRUE,row.names=1)
mydata.mds <- metaMDS(mydata)


#View items in the list produced by metaMDS.
# mydata.mds$points: sample scores
# mydata.mds$dims: number of MDS axes or dimensions
# mydata.mds$stress: stress value of final solution
# mydata.mds$data: what was ordinated, including any
# transformations
# mydata.mds$distance: distance metric used
# mydata.mds$converged: whether solution converged or
# not (T/F)
# mydata.mds$tries: number of random initial configurations
# tried
# mydata.mds$species: scores of variables (species / taxa
# in ecology)
# mydata.mds$call: restates how the function was called
names(mydata.mds)



#View the results of the MDS, which will display several elements of the list detailed above,
#including how the function was called (call), the data set and any transformations used (data),
#the distance measure (distance), the number of dimensions (dims), the final stress value
#(stress), whether any convergent solution was achieved (converged), how many random initial
#configurations were tried (tries), plus whether scores have been scaled, centered, or rotated.
mydata.mds



#Extract sample and variable scores. The column numbers correspond to the MDS axes, so
#this will return as many columns as was specified with the k parameter in the call to
#metaMDS.
variableScores <- mydata.mds$species
sampleScores <- mydata.mds$points



#Plot sample and variable scores in same space. Open black circles correspond to samples
#and red crosses indicate taxa.
plot(mydata.mds)



#MDS plots can be customized by selecting either sites or species. Also, labels may be displayed
#instead of symbols by specifying type="t".
plot(mydata.mds, type="t", display=c("species"))



#MDS plots can be further customized. By specifying type ="n", no sample scores or variable
#scores will be plotted. These can then be plotted with the points() and text() commands.
#For crowded plots, congestion of points and labels can be alleviated by plotting to a larger
#window and by using cex to reduce the size of symbols and text.
# plots axes, but no symbols or labels
plot(mydata.mds, type="n")



# pch=3, col="red")
# plots points for all samples (specified by "sites") for MDS
# axes 1 & 2 (specified by choices). Note that symbols can
# be modified with typical adjustments, such as pch and col.
# See the select parameter on help page for metaMDS for how to
# use a vector to specify which points are plotted.
points(mydata.mds, display=c("sites"), choices=c(1,2), pch=3, col="red")


# plots labels for all variable scores (specified by
# "species") for MDS axes 1 & 2. Typical plotting parameters
# can also be set, such as using cex to plot smaller labels.
# See labels parameter on help page for metaMDS for how to
# specify an alternative vector of labels, as opposed to the
# row or column names, which are used by default. Note that
# there is also a select parameter for text, as there is for
# points, mentioned above.
text(mydata.mds, display=c("species"), choices=c(1,2), col="blue", cex=0.7)



#--------stressplot and ordiplot----------
stressplot(mydata.mds)
ordiplot(mydata.mds,type="t")
