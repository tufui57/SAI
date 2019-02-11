
library(raster)
require(ggplot2)
require(reshape2)

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

# Function
plot_SAI <- function(scores, sai, time){
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  # Colour gradient for raster
  colfunc <- colorRampPalette(c("cyan", "red"))
  

  myplot <- ggplot(sai.dat, aes_string("x", "y", fill = time)) + 
    geom_raster() +
    scale_fill_gradientn(colours = colfunc(30), na.value = "transparent",
                         breaks=c(0, 0.5, 1),
                         limits=c(0,1)
    ) +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=20)
    )
  return(myplot)

}


#################################################################################
### Draw a map of spatial availability index of LGM area
#################################################################################

### Load data of cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

# # Load SAI values
load("SAI_5km_LGMInLGM_1500kmWindow_4var.data")
nam <- load("SAI_5km_LGMInLGM_1500kmWindow_4var.data")
sai <- get(nam)

time="SAIll"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""))

plot_SAI(scores.lgm, sai, time)

dev.off()


#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentIncurrent_1500kmWindow_4var.data")
sai <- load("SAI_5km_currentIncurrent_1500kmWindow_4var.data")
sai <- get(sai)

time="SAIcc"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""))

plot_SAI(scores, sai, time)

dev.off()

#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentInLGM_1500kmWindow_4var.data")
sai <- load("SAI_5km_currentInLGM_1500kmWindow_4var.data")
sai <- get(sai)

time="SAIcl"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""))

plot_SAI(scores, sai, time)

dev.off()