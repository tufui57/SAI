
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)

#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

# ### Load 1km data
# load(".\\Scores_landcover1km.data")
### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
# load("SAI_south_1km_current_4var.data")
# Load 5km SAI of current climate at the LGM
load("SAI_5km_LGM_PC1_2.data")

# Resolution of grid data (km)
reso = 5 

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref5 <- raster(paste("Y:\\GIS map and Climate data\\newzealandpotentialvegetatio", reso, ".bil", sep=""))

# Combine SAI to coordinate data
sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
colnames(sai.dat)[3] <- "AUC"

# Plot simialrity levels on NZ map
SAIraster <- convert_dataframe_to_raster(ref = ref5, dat = sai.dat,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "AUC")

# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))

# Map SAI
png("Y:\\SAI_5km.png")

plot(SAIraster,
     col=colfunc(21),
     axes=FALSE, box=FALSE,
     legend.args=list(text='SAI', side=4, font=2, line=2.5, cex=0.8)
)

dev.off()
