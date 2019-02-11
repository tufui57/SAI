
library(raster)
require(ggplot2)
require(reshape2)

#################################################################################
### Draw a map of spatial availability index of current area
#################################################################################

# ### Load 1km data
# load(".\\Scores_landcover1km.data")
### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentIncurrent_1500kmWindow_4var.data")
sai <- load("SAI_5km_currentIncurrent_1500kmWindow_4var.data")
sai <- get(sai)


# Resolution of grid data (km)
reso = 5 

# Combine SAI to coordinate data
sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
colnames(sai.dat)[3] <- "SAIcc"


# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "red"))

# Map SAI
png("Y:\\SAI_5km_current.png")

ggplot(sai.dat, aes(x, y, fill = SAIcc)) + 
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

dev.off()
