source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)
library(dplyr)
library(dismo)

# Load SAI values
load("SAI_south_1km_current_4var.data")

### Load 1km data
scores <- read.csv(".\\current_south_island_climate1km.csv")
sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
colnames(sai.dat)[3] <- "AUC"

###################################################################################################
### Calculate MESS
###################################################################################################

coordinateNames <- c("bioclim10", "bioclim11", "bioclim18", "bioclim19")


# NZ Geographycal location raster
ref <- raster(paste(".\\current_landcover1km.bil", sep=""))

r1 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                            coordinateCols = c("x", "y"),
                            cellvalueCol = coordinateNames[1])
r2 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                                  coordinateCols = c("x", "y"),
                                  cellvalueCol = coordinateNames[2])
r3 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                                  coordinateCols = c("x", "y"),
                                  cellvalueCol = coordinateNames[3])
r4 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                                  coordinateCols = c("x", "y"),
                                  cellvalueCol = coordinateNames[4])

s <- stack(r1,r2,r3,r4)
names(s) <- coordinateNames


# values of reference points (training data for the model, MESS)
refpt <- scores[, coordinateNames]

# Calculate MESS
ms <- mess(s, refpt, full=TRUE)
plot(ms)

# Extract MESS by points
mess_SI <- extract(ms[[5]], sai.dat[, c("x","y")])

sai.dat$mess <- mess_SI

###################################################################################################
### Calculate Mahalanobis Distance
###################################################################################################

### Mahalanobis distance; a measure of the distance between each observation in a multidimensional cloud of points and the centroid of the cloud. 

### Load 1km data
scores <- read.csv(".\\current_south_island_climate1km.csv")
coordinateNames <- c("bioclim10", "bioclim11", "bioclim18", "bioclim19")

mat <- as.matrix(scores[, coordinateNames])
scaled.mat <- scale(mat)

scores.cov <- cov(scaled.mat)
maha <- mahalanobis(scaled.mat, colMeans(scaled.mat), scores.cov)

### Mahalanobis similarity = Mahalanobis distance * (-1)
sai.dat$maha <- maha^2 * (-1)

# Plot simialrity levels on NZ map
maha.raster <- convert_dataframe_to_raster(ref = ref, dat = sai.dat,
                                         coordinateCols = c("x", "y"),
                                         cellvalueCol = "maha")

plot(maha.raster)

###################################################################################################
### Extract elevation for 1km grid resolution raster
###################################################################################################

library(raster)

### Load elevation data
ele <- raster(".//kx-nz-80m-digital-elevation-model-GTiff//nztm.tif")

### Load 1km data
scores <- read.csv(".\\current_south_island_climate1km.csv")
coordinateNames <- c("bioclim10", "bioclim11", "bioclim18", "bioclim19")

# Extract elevation by locations of centre of 1km grid cells
elev <- extract(ele, scores[, c("x","y")])

sai.dat$elev <- elev

write.csv(sai.dat, ".\\SAI_SI.csv")


