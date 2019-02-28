
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

# Function
plot_SAI <- function(scores, sai, time, colfunc){
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  ras <- convert_dataframe_to_raster(ref.raster, sai.dat, c("x","y"), time)
  
  plot(ras, col = colfunc(11),
       breaks = seq(0, 1, 0.1)
  )
  
  
}

hist_SAI <- function(scores, sai, time){
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  hist(sai.dat[,time], data = sai.dat,
       xlim=c(0,1), xlab = time, main="")
  
}


#### Set colour gradient
colfunc <- colorRampPalette(c("brown", "yellow", "green", "cyan", "blue", "violet", "red"))

source(".//functions//F02_create_raster_from_dataframe.R")
ref.raster <- raster("Y://GIS map and Climate data//LGM_NZTM_bioclim1.tif")



#################################################################################
### Draw a map of SAI of current climate at the LGM
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentInLGM_5000kmWindow_4var.data")
sai <- load("SAI_5km_currentInLGM_5000kmWindow_4var.data")
sai <- get(sai)

time="SAIcl"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""), width = 900, height = 630)

layout(matrix(c(1,1,2), nrow = 1, ncol = 3, byrow = TRUE))

plot_SAI(scores, sai, time, colfunc)
hist_SAI(scores, sai, time)

dev.off()
