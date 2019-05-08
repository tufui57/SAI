###################################################################################################
### Diagram showing how to calculate Spatial Availabiilty Index
###################################################################################################

source(".\\SAI\\F_SpatialAvailabilityIndex.R")
library(rgdal)
library(raster)
###################################################################################################
### Extract elevation for 1km grid resolution raster
###################################################################################################

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

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)

# Crop the extent
nzland2 <- crop(nzland, extent(ref))

###################################################################################################
### Plot the point on map
###################################################################################################

i = 10000 
p <- scores[i, ]

plot(prast)
points(p[, c("x","y")], pch = 19, col = "red")

###################################################################################################
### Plot the point on climate space
###################################################################################################

scores$bioclim1 <- scores$bioclim1/10

ggplot(data= scores) +
  geom_point(aes(x= bioclim1, y = bioclim12, color = Elevation)) +
  geom_point(data = scores[i,], aes(x= bioclim1, y = bioclim12), color="red") +
  scale_colour_gradientn(colours = terrain.colors(1000)) +
     xlab("Annual mean temperature (\u00B0C)")+ 
     ylab("Annual precipitation (mm)") +
  theme_classic()


###################################################################################################
### Calculate SAI and draw AUC figure
###################################################################################################

## Load 5km data
load(".\\Scores_Acaena_landcover5km.data")
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

# Get climate ranges of cells within the neighbourhood window 
ranges <- lapply(coordinateNames, get_radius_size, dat = scores)
names(ranges) <- coordinateNames

### SAI calculation
neighbours.size <- cells_within_neighbourhood_multivariate(p, # a point at the centre of search area
                                                           scores, # data of points to be searched
                                                           ranges, # result of get_radius_size()
                                                           twicerange = T,
                                                           coordinateNames # column name for climate variable in p and dat2
)

# Calculate percentage of area within the neighbourhood over NZ
ratio <- lapply(2:length(ranges[[1]]),
                function(j){
                  # Find points of a group within neighbourhood of another group of points 
                  nrow(neighbours.size[[j]]) / nrow(scores)
                }
)
### Plot the AUC figure
plot(seq(0,100,10), c(0, unlist(ratio)),
     xlab = "Climate ranges (%)",
     ylab = "Ratio",
     )


### Calculate AUC (area under curve) for each current grid cells
res <- auc(c(0, 1:10)*0.1, c(0, ratio), type="spline")


