source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)
library(dplyr)
library(dismo)

# Load SAI values
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover5km.bil", sep=""))

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

###################################################################################################
### Calculate Euclidean similarity
###################################################################################################
###  Euclidean similarity = Average distance between cell i and all the other cells * -1
mat <- as.matrix(scores[, coordinateNames])
scaled.mat <- scale(mat)

similarity <- list()

for(i in 1:nrow(scaled.mat)){
  # t <- Sys.time()
  # print(t)
  p <- mat[i,]
  mat1 <- scaled.mat[-i,]
  
  EucDist <- sapply(1:nrow(mat1), function(j){
    dist(rbind(p, mat1[j,]))
  }
  ) 
  similarity[[i]] <- sum(EucDist)/nrow(mat1) * (-1)
  
}

scores$ES <- unlist(similarity)
write.csv(scores, ".\\Euclidean_similarity5km.csv")
