source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)
library(dplyr)
library(dismo)

# Load SAI values
load("SAI_south_1km_current_4var.data")
ref5 <- raster(paste("D:\\PhD\\GIS map and Climate data\\current_landcover1km.bil", sep=""))

### Load 1km data
scores <- read.csv("D:\\PhD\\current_south_island_climate1km.csv")
sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
colnames(sai.dat)[3] <- "AUC"

coordinateNames <- c("bioclim10", "bioclim11", "bioclim18", "bioclim19")

###################################################################################################
### Calculate Euclidean similarity
###################################################################################################
###  Euclidian similarity = Average distance between cell i and all the other cells * -1
mat <- as.matrix(scores[, coordinateNames])
scaled.mat <- scale(mat)

similarity <- list()

nrow(scaled.mat)

for(i in 1:10000){
  # t <- Sys.time()
  # print(t)
  p <- mat[i,]
  mat1 <- scaled.mat[-i,]
  
  similarity[[i]] <- sapply(1:nrow(mat1), function(j){
    dist(rbind(p, mat1[j,]))
  }
  ) %>% sum(.)/nrow(mat1) * (-1)
  
}

sai.dat$ES <- unlist(similarity)
write.csv(sai.dat, ".\\SAI_es.csv")