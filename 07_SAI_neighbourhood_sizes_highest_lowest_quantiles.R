############################################################################################################
## Find ratio of cells with SAI difference < 0 or > 0 
############################################################################################################

load("diff_SAIcc_cl_5km.data")

sum(sai.diff$diff < -0.025) / nrow(sai.diff)
sum(sai.diff$diff > 0) / nrow(sai.diff)


############################################################################################################
## Distributions of the lowest 25% of SAIs within 4 diferent neighbourhood size
############################################################################################################

library(dplyr)
library(ggplot2)
library(rgdal)
library(raster)
library(gridExtra)

### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(20,50,100))){
  a <- load(paste("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_",  i, "kmWindow_4var_climateRange_of_neighbourhood.data", sep=""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}

a <- load("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_5000kmWindow_4var.data")
a <- get(a)
sai.c[[4]] <- unlist(a)

sai.c2 <- data.frame(do.call(cbind,sai.c))
colnames(sai.c2)[4] <- "NZ"

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")


############################################################################################################
## Identify the cells with the lowest/highest 25% of SAIs within 4 diferent neighbourhood size
############################################################################################################

### The lowest 25% of SAI = SAI < 25% quantile
qu <- lapply(sai.c2, function(x){
  x < quantile(x, probs = seq(0, 1, 0.25))[2]
  }
)
qu25 <- do.call(cbind, qu)

qu25dat <- scores[rowSums(qu25) == 4, ]

### The highest 25% of SAI = SAI > 75% quantile
qu <- lapply(sai.c2, function(x){
  x > quantile(x, probs = seq(0, 1, 0.25))[3]
}
)
qu75 <- do.call(cbind, qu)

qu75dat <- scores[rowSums(qu75) == 4, ]


# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)

# Reference rasters
ref <- raster("Y://GIS map and Climate data//current_landcover1km.bil")

# Crop the extent
nzland2 <- crop(nzland, extent(ref))

##################
### Draw a map
##################

myplot25 <- ggplot(qu25dat, aes_string("x", "y")) + 
  geom_raster(fill = "blue") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )


myplot75 <- ggplot(qu75dat, aes_string("x", "y")) + 
  geom_raster(fill = "red") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

png("Y:\\SAI_25_75quntiles.png", width = 1000, height = 550)

# Plot in multiple panels
grid.arrange(myplot25, myplot75, ncol = 2)
dev.off()

