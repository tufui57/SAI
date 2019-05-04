###################################################################################
#############   Clip South Island polygon 
###################################################################################

source(".//functions//F_clip_raster_by_polygon.R")
source(".//functions//F02_create_raster_from_dataframe.R")
library(dplyr)
library(raster)

######################################################
# 1. Import polygons of current whole NZ
######################################################

# Import current outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)


##########################################################
# 3. Extract past climate rasters by polygons
##########################################################

### Calculate area of each polygon to find the largest island of LGM

areas <- sapply(slot(nzland, "polygons"), function(x){
  sapply(slot(x, "Polygons"), slot, "area")}
  )
land.areas <- lapply(areas, max) %>% unlist
max.area <- (land.areas == max(land.areas)) %>% which

# Plot single polygon
plot(nzland[max.area,])

# Save polygon of South Island (the largest island of current NZ)
sland <- as(nzland[max.area,], "SpatialPolygonsDataFrame")
writeOGR(sland, dsn = ".", layer = 'SouthIsland', driver = "ESRI Shapefile")


### Load 1km cliamte data
load(".\\Scores_landcover1km.data")
# load reference raster
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover1km.bil", sep=""))

# Add cell ID
scores$id <- 1:nrow(scores)

# COnvert dataframe to raster
ras1km <- convert_dataframe_to_raster(ref = ref5, dat = scores,
                            coordinateCols = c("x", "y"),
                            cellvalueCol = "id")

##########################################################
# Clip raster by polygon of South Island
##########################################################

southisland <- clip.by.polygon(ras1km, shape = nzland[max.area,])

# Save the clipped rasters
save(southisland, file = ".//SouthIsland.data")

### Extract South Island data by merge(, by = cell ID)
dat.south <- cbind(coordinates(southisland), values(southisland))
colnames(dat.south)[3] <- "id"

dat.south2 <- dat.south[!is.na(dat.south[,3]), ] %>% 
  merge(scores[, -which(colnames(scores) == "x" | colnames(scores) == "y")], ., by = "id")

# Save South Island data
write.csv(dat.south2, file = "Y:\\current_south_island_climate1km.csv")

