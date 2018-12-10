
source(".//F02_create_raster_from_dataframe.R")
library(raster)

#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

# Load 1km data
load(".\\Scores_landcover1km.data")

# Load SAI values
load("SAI_south_1km_current_4var.data")

# Reference raster of coordinate system & extent
ref5 <- raster(paste(".\\current_landcover1km.bil", sep=""))

# Bind SAI column to climate data
sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
colnames(sai.dat)[3] <- "AUC"

# Plot SAI on NZ map
SAIraster <- convert_dataframe_to_raster(ref = ref5, dat = sai.dat,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "AUC")

# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))

# Map SAI
png(".\\SAI_south_1km_current_20kmwindow.png")

plot(SAIraster,
     col=colfunc(21),
     axes=FALSE, box=FALSE,
     legend.args=list(text='SAI', side=4, font=2, line=2.5, cex=0.8)
)

dev.off()
