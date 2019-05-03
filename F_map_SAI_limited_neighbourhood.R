
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)

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
## Map SAI within limited neighbourhood size
############################################################################################################
### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(20,50,100))){
  a <- load(paste("SAI_5km_currentInCurrent_", i,"kmWindow_4var_climateRange_of_neighbourhood.data", sep=""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}

a <- load("SAI_5km_currentInCurrent_5000kmWindow_4var.data")
a <- get(a)
sai.c[[4]] <- unlist(a)

#################################################################################
### Draw a map of SAI of current climate at the current
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

time="SAIcc"

# Map SAI
for(i in 1:length(sai.c)){
  names(sai.c) <- c("20", "50", "100","5000")
  
  png(paste("Y:\\", time ,"_5km_", names(sai.c)[i], "km_neighbourhood.png", sep = ""), width = 900, height = 630)
  myplot <- plot_SAI(scores, sai.c[[i]], time, colfunc)
  myhist <- hist_SAI(scores, sai.c[[i]], time)
  
  # Plot in multiple panels
  grid.arrange(myplot, myhist,
               ncol = 2, nrow = 1, widths = c(2, 1))
  dev.off()
}
