
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)
library(cowplot)

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

### Function to Draw the SAI map
plot_SAI <- function(scores, sai, time, colfunc){
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  # Set colour scale 
  sai.dat$brks <- cut(sai.dat[,time], 
                      breaks = seq(0, 1, 0.1), 
                      labels=c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3","0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                               "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))
  
  # Draw the map
  myplot <- ggplot(sai.dat, aes_string("x", "y", fill = "brks")) + 
    geom_raster() +
    scale_fill_manual(time, values=setNames(colfunc(11), levels(sai.dat$brks)), na.value = "transparent"
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

#### Function to draw Histgram of the SAI values
hist_SAI <- function(scores, sai, time){
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  myhist <- ggplot(sai.dat, aes_string(x = time)) +
    geom_histogram(data = sai.dat, bins = 100) +
    xlim(0,1) +
    xlab(time) +
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


############################################################################################################
## Visualize SAI without outliers
############################################################################################################
### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

#################################################################################
### SAIcc without X % of outliers
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
### Draw three maps of SAI of current climate at the current in one panel
#################################################################################
time = "SAIcc"
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

