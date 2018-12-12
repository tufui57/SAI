###################################################################################################
### Extract elevation for 1km grid resolution raster
###################################################################################################

library(raster)

### Load elevation data
ele <- raster("D://PhD//GIS map and Climate data//kx-nz-80m-digital-elevation-model-GTiff//nztm.tif")

### Load 1km data
scores <- read.csv("D:\\PhD\\current_south_island_climate1km.csv")
coordinateNames <- c("bioclim10", "bioclim11", "bioclim18", "bioclim19")

# Extract elevation by locations of centre of 1km grid cells
elev <- extract(ele, scores[, c("x","y")])

scores$elev <- elev

write.csv("D:\\PhD\\current_south_island_climate1km_elev.csv")