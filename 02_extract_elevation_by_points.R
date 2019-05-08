###################################################################################################
### Extract elevation for 1km grid resolution raster
###################################################################################################

library(raster)

## Load 5km data
load(".\\Scores_Acaena_landcover5km.data")
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

### Load elevation data
ele <- raster("Y://GIS map and Climate data//kx-nz-80m-digital-elevation-model-GTiff//nztm.tif")

# Extract elevation by locations of centre of 1km grid cells
scores$Elevation<- extract(ele, scores[, c("x","y")])

# Reference rasters
ref <- raster("Y://GIS map and Climate data//current_landcover5km.bil")
# create new raster having the same dimentions as reference raster (ex. pre-human map)
rast <- raster(ncol = ncol(ref), nrow = nrow(ref))
extent(rast) <- extent(ref)

pts <- scores[, c("x", "y")]

# point coordinate system setting
coordinates(pts) <- scores[, c("x", "y")]
proj4pts <- proj4string(ref)
proj4string(pts) <- CRS(proj4pts)
# land use change column
pts$elevation <- scores$Elevation

# rasterize
prast <- rasterize(pts, ref, field = pts$elevation, fun = mean)

###################################################################################################
### Plot the map
###################################################################################################

plot(prast)