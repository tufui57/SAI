
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
  
  
  sai.dat$brks <- cut(sai.dat[,time], 
                     breaks = seq(0, 1, 0.1), 
                     labels=c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3","0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                              "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))
  
  
  
  
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


#################################################################################
### Draw a map of SAI of LGM climate at the LGM
#################################################################################

### Load data of cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

# # Load SAI values
load("SAI_5km_LGMInLGM_5000kmWindow_4var.data")
nam <- load("SAI_5km_LGMInLGM_5000kmWindow_4var.data")
sai <- get(nam)

time="SAIll"

# Map SAI
png(paste("Y:\\", time ,"_5kmtest.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores.lgm, sai, time, colfunc)
myhist <- hist_SAI(scores.lgm, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()


#################################################################################
### Draw a map of SAI of current climate at the current
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai <- load("SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai <- get(sai)

time="SAIcc"

# Map SAI
png(paste("Y:\\", time ,"_5kmtest.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai, time, colfunc)
myhist <- hist_SAI(scores, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()

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
png(paste("Y:\\", time ,"_5kmtest.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai, time, colfunc)
myhist <- hist_SAI(scores, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))

dev.off()

#################################################################################
### Draw a map of the difference of SAIcc - SAIcl
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("diff_SAI_5km_wholeNZ27Feb.data")
sai <- load("diff_SAI_5km_wholeNZ27Feb.data")
sai <- get(sai)

# Check max/min of difference of SAI for colour scale
summary(sai$diff)

# Set breaks to draw in discrete colour scale for continuous values
sai$brks <- cut(sai$diff,
                breaks = seq(-0.1, 0.125, 0.025), 
                labels = c("-0.100 - -0.075", "-0.075 - -0.050",
                           "-0.050 - -0.025", "-0.025 - 0.000", "0.000 - 0.025", "0.025 - 0.050","0.050 - 0.075",
                           "0.075 - 0.100","0.100 - 0.125"
                           )
                )

# Colour functions for > 0 and < 0
col.up <- colorRampPalette(c("violet", "red"))
col.down <- colorRampPalette(c("blue", "lightblue"))


library(rgdal)
# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)

# Reference rasters
ref <- raster("Y://GIS map and Climate data//current_landcover1km.bil")

# Crop the extent
nzland2 <- crop(nzland, extent(ref))

# Map SAI
png("Y:\\diff_SAIcc_SAIcl_5kmtest.png", width = 900, height = 630)

myplot <- ggplot(sai, aes_string("x", "y", fill = "brks")) + 
  geom_raster() +
  scale_fill_manual("SAIcc-SAIcl", 
                    values=setNames(c(col.down(3), rep("white",2),col.up(5)), levels(sai$brks)),
                    na.value = "transparent"
  ) +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=15)
  )

myhist <- ggplot(sai, aes_string(x = "diff")) +
  geom_histogram(data = sai, bins = 100) +
  xlim(-0.1, 0.125) +
  xlab("difference") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 270, vjust = 0.25),
        axis.title.y = element_text(angle = 270),
        axis.ticks.y = element_blank()
  ) +
  theme(panel.background = element_rect(fill = 'gray96'))


# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))


dev.off()


