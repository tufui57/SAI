### Comparison of indices
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)
library(cowplot)

# Correlation matrix
setwd("Y://5th chapter SAI chapter//meta data")

# Load spatial similarity indices
sai <- load("SAI_5km_currentInCurrent_5000kmWindow_4var.data")
sai <- get(sai)
load("Y://5th chapter SAI chapter//raw data//Scores_Acaena_landcover5km.data")
scores.sai <- cbind(scores, unlist(sai))
colnames(scores.sai)[length(scores.sai)] <- "SAI"

mess <- read.csv("MESS5km.csv")
colnames(mess)[length(mess)] <- "MESS"
maha <- read.csv("Mahalanobis5km.csv")
colnames(maha)[length(maha)] <-"Mahalanobis"
euc <- read.csv("Euclidean_similarity5km.csv")
colnames(euc)[length(euc)] <- "Euclidean"

# Merge all indices
dat <- merge(mess[,c("x", "y", "MESS")], maha[,c("x", "y", "Mahalanobis")], by=c("x","y"))
dat2 <- merge(dat[,c("x", "y", "MESS", "Mahalanobis")], euc[,c("x", "y", "Euclidean")], by=c("x","y"))

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

### Function to Draw the SAI map
plot_similarityIndex <- function(data, index, colfunc){
  # Combine index to coordinate data
  index.dat <- data[,c("x", "y", index)]
  colnames(index.dat)[3] <- index
  
  # Set colour scale 
  breaks.index <- seq(min(index.dat[,index]), max(index.dat[,index]), 
                                   (max(index.dat[,index])-min(index.dat[,index]))/10)
  index.dat$brks <- cut(index.dat[,index], 
                      breaks = breaks.index,
                      labels = c(paste(round(breaks.index[1],2), "-", round(breaks.index[2],2)), " ","  ","   ","    ", 
                                paste(round(breaks.index[6],2), "-", round(breaks.index[7],2)), "     ","       ", "        ",
                                paste(round(breaks.index[10],2), "-", round(breaks.index[11],2))
                      )
                      )
  
  # Draw the map
  myplot <- ggplot(index.dat, aes_string("x", "y", fill = "brks")) + 
    geom_raster() +
    scale_fill_manual(index, values=setNames(colfunc(11), levels(index.dat$brks)), na.value = "transparent"
    ) +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          axis.line        = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=20)
    )
  return(myplot)
  
}

#### Function to draw Histgram of the index values
hist_similarityIndex <- function(data, index){
  
  # Combine index to coordinate data
  index.dat <- data[,c("x", "y", index)]
  colnames(index.dat)[3] <- index
  
  myhist <- ggplot(index.dat, aes_string(x = index)) +
    geom_histogram(data = index.dat, bins = 100) +
    xlim(min(index.dat[,index]), max(index.dat[,index])) +
    xlab(index) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 270, vjust = 0.25),
          axis.title.y = element_text(angle = 270),
          axis.ticks.y = element_blank()
    ) +
    theme(panel.background = element_rect(fill = 'gray96'))
  
  return(myhist)
  
}


#### Set colour gradient
colfunc <- colorRampPalette(c("brown", "yellow", "green", "cyan", "blue", "violet", "red"))

#################################################################################
### Draw three maps of SAI of current climate at the current in one panel
#################################################################################

myplot <- list()
myhist <- list()

for(i in c("Euclidean","MESS", "Mahalanobis")){
  
  myplot[[i]] <- plot_similarityIndex(dat2, i, colfunc) + 
    theme(legend.position = "none") +
    # Leave space on left to add legend manually later
    theme(plot.margin=unit(c(1,3,1,1),"cm"))
  
  myhist[[i]] <- hist_similarityIndex(dat2, i)
  
}

png("Y:\\SimilarityIndices_5km_neighbourhood.png", width = 1200, height = 550)

# Plot in multiple panels
grid.arrange(
  arrangeGrob(myplot[[1]],myplot[[2]],myplot[[3]],
              myhist[[1]],myhist[[2]],myhist[[3]],
              ncol = 3, nrow = 2, heights = c(3, 1))
)
dev.off()

#################################################################################
### Add scalle bar and north mark on a map within the above panel
#################################################################################

source("Y:\\1st chapter_Acaena project\\Acaena manuscript\\scripts\\Fig01_2_add_scaleBar_direction_on_Land_cover_change_map.r")

tm_shape(brick(prast)) + 
  tm_raster(alpha = 0.7) +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)+
  tmap_save(filename="Y:\\scalbar and north mark on NZ map.png", 
            units = "in", width = 1000 * 8/27 * 0.0104166667, height = 550 * 4/5* 0.0104166667)



