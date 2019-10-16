
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

# Function
plot_SAI <- function(scores, sai, nameOfsai, 
                     coordinateNames, # the order must be c("x", "y")
                     colfunc){
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, coordinateNames], unlist(sai))
  colnames(sai.dat)[3] <- nameOfsai
  
  sai.dat$brks <- cut(sai.dat[,nameOfsai], 
                      breaks = seq(0, 1, 0.1), 
                      labels=c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3","0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                               "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0")
  )
  
  myplot <- ggplot(sai.dat, 
                   aes_string(coordinateNames[1], coordinateNames[2], fill = "brks")
  ) + 
    geom_raster() +
    scale_fill_manual(nameOfsai, values=setNames(colfunc(11), levels(sai.dat$brks)), na.value = "transparent"
    ) +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=20)
    )
  return(myplot)
  
}

hist_SAI <- function(scores, sai, nameOfsai){
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- nameOfsai
  
  myhist <- ggplot(sai.dat, aes_string(x = nameOfsai)) +
  geom_histogram(data = sai.dat, bins = 100) +
  xlim(0,1) +
  xlab(nameOfsai) +
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
### Draw a map of SAI of current climate at the current
#################################################################################

### Load 5km data
load("Y:\\Scores_LGM_mainisland_worldclim1_5km.data")

# Load SAI values
load("Y://EPcl_20kmNeighbourhood.data")
sai <- load("Y://EPcl_20kmNeighbourhood.data")
sai <- get(sai)

nameOfsai="EPcl"

# Map SAI
png(paste("Y:\\", nameOfsai ,"_20km_test.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai$EP, nameOfsai, coordinateNames = c("x","y"), colfunc)
myhist <- hist_SAI(scores, sai$EP, nameOfsai)

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
load(".\\SAIcl_NZ_p.data")
sai <- load(".\\SAIcl_NZ_p.data")
sai <- get(sai)

nameOfsai="SAIcl"

# Map SAI
png(paste("Y:\\", nameOfsai ,"_5kmtest.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai, nameOfsai, coordinateNames = c("x","y"), colfunc)
myhist <- hist_SAI(scores, sai, nameOfsai)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))

dev.off()
