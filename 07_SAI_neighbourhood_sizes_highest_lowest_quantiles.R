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

quntile_top_SAI <- function(percent
){
  ### The highest 25% of SAI = SAI > 75% quantile
  qu <- lapply(sai.c2, function(x){
    x > quantile(x, probs = seq(0, 1, percent/100 ))[length(seq(0, 1, percent/100)) - 1]
  }
  )
  qu_top <- do.call(cbind, qu)
  
  qu_top_dat <- scores[rowSums(qu_top) == 4, ]
  
  return(qu_top_dat)
}

quntile_bottom_SAI <- function(percent
){
  ### The lowest 25% of SAI = SAI < 25% quantile
  qu <- lapply(sai.c2, function(x){
    x < quantile(x, probs = seq(0, 1, percent/100 ))[2]
  }
  )
  qu_bot <- do.call(cbind, qu)
  
  qu_bot_dat <- scores[rowSums(qu_bot) == 4, ]
  
  return(qu_bot_dat)
}
############################################################################################################
## Identify the cells with the lowest/highest 25% of SAIs within 4 diferent neighbourhood size
############################################################################################################

### The lowest/highest 25% of SAI = SAI < 25% quantile
qu_top25 <- quntile_top_SAI(25)
qu_bot25 <- quntile_bottom_SAI(25)
res <- list(qu_top25, qu_bot25)

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

myplot25 <- ggplot(res[[2]], aes_string("x", "y")) + 
  geom_raster(fill = "blue") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )


myplot75 <- ggplot(res[[1]], aes_string("x", "y")) + 
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


############################################################################################################
## Identify the cells with the lowest 25% of SAI within 20 km and highest 25% of SAI within NZ
############################################################################################################

data <- cbind(scores, sai.c2)

quntile_bot_SAI <- function(percent,
                            data, # 
                            neigh # colum name of neighbourhood size
){
  ### The lowest 25% of SAI = SAI < 25% quantile
  qu <- (data[,neigh] < quantile(data[,neigh], probs = seq(0, 1, percent/100 ))[2])
  
  qu_bot_dat <- data[qu, ]
  return(qu_bot_dat)
}

quntile_top_SAI <- function(percent,
                            data, # 
                            neigh # colum name of neighbourhood size
){
  ### The highest 25% of SAI = SAI > 75% quantile
  qu <- (data[,neigh] > quantile(data[,neigh], probs = seq(0, 1, percent/100 ))[length(seq(0, 1, percent/100)) - 1])
  qu_top_dat <- data[qu, ]

  return(qu_top_dat)

}


## Areas with locally rare but nationally common climate
quLR <- merge(quntile_top_SAI(25, data, "NZ"), quntile_bot_SAI(25, data, "X20"), by=c("x","y"))

myplot1 <- ggplot(quLR, aes_string("x", "y")) + 
  geom_raster(fill = "brown") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  ggtitle("Locally rare but nationally common climates") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

############################################################################################################
## Identify the cells with the highest 25% of SAI within 20 km and lowest 25% of SAI within NZ
############################################################################################################

## Areas with locally rare but nationally common climate
quLC <- merge(quntile_top_SAI(25, data, "X20"), quntile_bot_SAI(25, data, "NZ"), by=c("x","y"))

myplot2 <- ggplot(quLC, aes_string("x", "y")) + 
  geom_raster(fill = "brown") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  ggtitle("Locally common but nationally rare climates") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

############################################################################################################
## Identify the cells with the lowest 25% of SAI within 20 km and NZ
############################################################################################################

## Areas with locally and nationally rare climate
quLC <-  merge(quntile_top_SAI(25, data, "X20"), quntile_top_SAI(25, data, "NZ"), by=c("x","y"))

myplot3 <- ggplot(quLC, aes_string("x", "y")) + 
  geom_raster(fill = "brown") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  ggtitle("Locally and nationally common climates") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

############################################################################################################
## Identify the cells with the highest 25% of SAI within 20 km and NZ
############################################################################################################

## Areas with locally and nationally rare climate
quLC <-  merge(quntile_bot_SAI(25, data, "X20"), quntile_bot_SAI(25, data, "NZ"), by=c("x","y"))

myplot4 <- ggplot(quLC, aes_string("x", "y")) + 
  geom_raster(fill = "brown") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  ggtitle("Locally and nationally rare climates") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )


############################################################################################################
## Plot locally/nationally rare/common climate
############################################################################################################

png("Y:\\SAI_locallynatinally.png", width = 1000, height = 1100)
# Plot in multiple panels
grid.arrange(myplot1, myplot4, 
             myplot3, myplot2,
             nrow = 2, ncol = 2)
dev.off()
