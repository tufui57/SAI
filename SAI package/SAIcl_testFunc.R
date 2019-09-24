library(sp)
library(rgdal) 
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)


load(".\\SAI_5km_currentInCurrent_20kmWindow_4var_climateRange_of_neighbourhood.data")
# Map SAI
time="SAIcc"
png(paste("Y:\\SAIcc_20km_inThesis.png", sep = ""), width = 900, height = 630)
myplot <- plot_SAI(scores, sai.i, time, colfunc)
myhist <- hist_SAI(scores, sai.i, time)
# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()


load(paste(".\\SAI_5km_currentInCurrent_20kmNeighbourhood.data", sep=""))
# Map SAI
time="SAIcc"
png("Y:\\SAIcc_20km_neighbourhood.png", width = 900, height = 630)
myplot <- plot_SAI(scores, sai.i, time, colfunc)
myhist <- hist_SAI(scores, sai.i, time)
  
# Plot in multiple panels
grid.arrange(myplot, myhist, ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()

