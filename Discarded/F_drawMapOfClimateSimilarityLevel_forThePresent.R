
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)

#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

# Load SAI values
load("spatialAvailabilityIndex_5km_of_current_on_PCA.data")
# Resolution of grid data (km)
reso=5 

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))

# Plot simialrity levels on NZ map
similarityLevelRaster <- convert_dataframe_to_raster(ref = ref5, dat = scores,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "AUC")

# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))

# Map SAI
png("Y:\\SAIcurrent.png")

plot(similarityLevelRaster,
     col=colfunc(21),
     axes=FALSE, box=FALSE,
     legend.args=list(text='SAI', side=4, font=2, line=2.5, cex=0.8)
)

dev.off()



#################################################################################
### Draw a map of spatial availability index of LGM land area
#################################################################################

# Load SAI values
load("spatialAvailabilityIndex_5km_of_LGM_on_PCA_withinLGMland.data")
# Resolution of grid data (km)
reso=5 

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))
# Load LGM climate data
load(".//LGMclimate.data")
extent(ref5) <- extent(lgm.mainland[[1]])

# Plot simialrity levels on NZ map
similarityLevelRaster <- convert_dataframe_to_raster(ref = ref5, dat = scores.lgm2,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "AUC")

# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))

# Map SAI
png("Y:\\SAI15oct.png")

plot(similarityLevelRaster,
     col=colfunc(21),
     axes=FALSE, box=FALSE,
     legend.args=list(text='SAI', side=4, font=2, line=2.5, cex=0.8)
     )

dev.off()



