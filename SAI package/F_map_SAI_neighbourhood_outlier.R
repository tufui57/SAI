
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)
library(cowplot)


############################################################################################################
## Map SAI within limited neighbourhood size
############################################################################################################
### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(20,50,100))){
  a <- load(paste(".\\SAI_5km_currentInCurrent_", i,"kmNeighbourhood.data", sep=""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}

a <- load(".\\SAIcc_NZ_4var.data")
a <- get(a)
sai.c[[4]] <- unlist(a)

#################################################################################
### Draw a map of SAIcc within different neighbourhood
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

time="SAIcc"

names(sai.c) <- c("20", "50", "100","5000")

# Map SAI
for(i in 1:length(sai.c)){
  png(paste("Y:\\SAIcc_5km_", names(sai.c)[i], "km_neighbourhood.png", sep = ""), width = 900, height = 630)
  myplot <- plot_SAI(scores, sai.c[[i]], time, colfunc)
  myhist <- hist_SAI(scores, sai.c[[i]], time)
  
  # Plot in multiple panels
  grid.arrange(myplot, myhist,
               ncol = 2, nrow = 1, widths = c(2, 1))
  dev.off()
}
#################################################################################
### Draw three maps of SAIcc within three different neighbourhood in one panel
#################################################################################

myplot <- list()
myhist <- list()
names(sai.c) <- c("20", "50", "100","5000")

for(i in 1:3){
  
  myplot[[i]] <- plot_SAI(scores, sai.c[[i]], time, colfunc) + theme(legend.position = "none")
  
  myhist[[i]] <- hist_SAI(scores, sai.c[[i]], time)

}

### Get just color scale bar
legend.plot <- plot_SAI(scores, sai.c[[1]], time, colfunc)
legend <- cowplot::get_legend(legend.plot)

### Get empty plot
empty.plot <- ggplot(scores, aes(x = bioclim1, y = bioclim2)) + geom_blank() + 
  theme_void()

png("Y:\\SAIcc_5km_neighbourhood.png", width = 1000, height = 550)

# Plot in multiple panels
grid.arrange(
  arrangeGrob(myplot[[1]],myplot[[2]],myplot[[3]],legend,
              myhist[[1]],myhist[[2]],myhist[[3]], empty.plot,
    ncol = 4, nrow = 2, heights = c(4, 1), widths = c(8,8,8,3))
  )
dev.off()

#################################################################################
### Add a scale bar and north mark on a map within the above panel
#################################################################################

source("Y:\\1st chapter_Acaena project\\Acaena manuscript\\scripts\\Fig01_2_add_scaleBar_direction_on_Land_cover_change_map.r")

tm_shape(brick(prast)) + 
  tm_raster(alpha = 0.7) +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)+
  tmap_save(filename="Y:\\scalbar and north mark on NZ map.png", 
            units = "in", width = 1000 * 8/27 * 0.0104166667, height = 550 * 4/5* 0.0104166667)



#################################################################################
### Draw a map of SAIcc without X % of outliers
#################################################################################
sai.list <- list()

# Load SAIcc
saiwith <- load("Y://5th chapter SAI chapter//meta data//SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai.list[["0"]] <- get(saiwith)


for(i in as.character(c(1, 2.5, 5))){
  sai <- load(paste("SAI_5km_currentInCurrent_5000kmWindow_4var_outlier", i, ".data", sep=""))
  res <- get(sai)
  
  sai.list[[i]] <- unlist(res)
}

sai.list2 <- data.frame(do.call(cbind,sai.list))

#################################################################################
### Draw three maps of SAIcc without outliers in one panel
#################################################################################

myplot <- list()
myhist <- list()

for(i in 1:4){
  
  myplot[[i]] <- plot_SAI(scores, sai.list2[[i]], time, colfunc) + theme(legend.position = "none")
  
  myhist[[i]] <- hist_SAI(scores, sai.list2[[i]], time)
  
}

### Get just color scale bar
legend.plot <- plot_SAI(scores, sai.list2[[1]], time, colfunc)
legend <- cowplot::get_legend(legend.plot)

### Get empty plot
empty.plot <- ggplot(scores, aes(x = bioclim1, y = bioclim2)) + geom_blank() + 
  theme_void()

png("Y:\\SAIcc_outliers.png", width = 1000, height = 550)

# Plot in multiple panels
grid.arrange(
  arrangeGrob(myplot[[2]],myplot[[3]],myplot[[4]],legend,
              myhist[[2]],myhist[[3]],myhist[[4]], empty.plot,
              ncol = 4, nrow = 2, heights = c(4, 1), widths = c(8,8,8,3))
)
dev.off()
