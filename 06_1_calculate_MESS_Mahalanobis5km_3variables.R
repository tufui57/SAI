source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)
library(dplyr)
library(dismo)


### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12")

###################################################################################################
### Calculate MESS
###################################################################################################

# Raster of geographycal map of NZ
ref <- raster(paste("Y:\\GIS map and Climate data\\current_landcover5km.bil", sep=""))

r1 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                            coordinateCols = c("x", "y"),
                            cellvalueCol = coordinateNames[1])
r2 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                                  coordinateCols = c("x", "y"),
                                  cellvalueCol = coordinateNames[2])
r3 <- convert_dataframe_to_raster(ref = ref, dat = scores,
                                  coordinateCols = c("x", "y"),
                                  cellvalueCol = coordinateNames[3])

s <- stack(r1,r2,r3)
names(s) <- coordinateNames

# values of reference points (training data for the model, MESS)
refpt <- scores[, coordinateNames]

# Calculate MESS
ms <- mess(s, refpt, full=TRUE)
plot(ms)

ms2 <- data.frame(coordinates(ms[[4]]), values(ms[[4]]))

# Save MESS
write.csv(ms2, "MESS5km_3variables.csv")

###################################################################################################
### Calculate Mahalanobis Distance
###################################################################################################

### Mahalanobis distance; a measure of the distance between each observation in a multidimensional cloud of points and the centroid of the cloud. 

mat <- as.matrix(scores[, coordinateNames])
scaled.mat <- scale(mat)

scores.cov <- cov(scaled.mat)
maha <- mahalanobis(scaled.mat, colMeans(scaled.mat), scores.cov)

### Mahalanobis similarity = Mahalanobis distance * (-1)
scores$maha <- maha^2 * (-1)

# Plot simialrity levels on NZ map
maha.raster <- convert_dataframe_to_raster(ref = ref, dat = scores,
                                         coordinateCols = c("x", "y"),
                                         cellvalueCol = "maha")

plot(maha.raster)
points(scores[which(scores$maha == min(scores$maha)), c("x","y")],
       pch = 10
       )

# Save MESS
write.csv(scores, "Mahalanobis5km_3variables.csv")
