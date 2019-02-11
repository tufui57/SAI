
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)

#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

### Load data of cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

# # Load SAI values
load("SAI_5km_LGMInLGM_20kmWindow_4var.data")
nam <- load("SAI_5km_LGMInLGM_20kmWindow_4var.data")
sai <- get(nam)

# Reference raster of coordinate system & extent
load(".//LGMclimate.data")
# Resample raster from 1km to 5km
ref.raster <- raster("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil")
# Adjust extent
extent(ref.raster) <- extent(lgm.mainland[[1]])
ref5 <- resample(lgm.mainland[[1]], ref.raster)

# Combine SAI to coordinate data
sai.dat <- cbind(scores.lgm[, c("x", "y")], unlist(sai))
colnames(sai.dat)[3] <- "AUC"

# Plot simialrity levels on NZ map
SAIraster <- convert_dataframe_to_raster(ref = ref5, dat = sai.dat,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "AUC")

# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))

# Map SAI
png("Y:\\SAI20_LGM_5km.png")

plot(SAIraster,
     col=colfunc(22),
     axes=FALSE, box=FALSE,
     legend.args=list(text='SAI', side=4, font=2, line=2.5, cex=0.8)
)

dev.off()
