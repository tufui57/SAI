################################################################
### Maps of the current and LGM bioclim
################################################################

library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)

################################################################
### Load NZ outlines
################################################################

# Reference raster
ref <- raster(paste("Y:\\GIS map and Climate data\\current_landcover1km.bil", sep = ""))

# the current outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)

# the LGM outline of NZ
path = ".\\LGM_mainIsland\\mainIsland.shp"
LAYERS <- ogrListLayers(path)
lgmland <- readOGR(path, LAYERS)

################################################################
### Data preparation
################################################################

# Load LGM climate data
load(".//LGMclimate.data")

# Load current climate data
alld <- read.csv("Y:\\5th chapter SAI chapter\\raw data\\Acaena_bioclim_landcover_history_worldclim1_1km.csv")

d <- alld[is.na(alld$bioclim1) == F, ]
### Extract coordinates and cliamte to plot
d2 <- d[, c("x", "y", "bioclim1", "bioclim6", "bioclim12", "bioclim15")]


################################################################
### Function to plot the bioclim map
################################################################

map_plot <- function(data, # data for map
                     colname.to.draw # character string. Colname of the data to colour
){
  
  pMap <- ggplot() +
    geom_raster(data = data, aes_string(x = "x", y = "y", fill = colname.to.draw)) +

    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha = 0.7))) +
    
    labs(x = "", y = "") +
    # Legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          plot.title = element_text(family = "Times New Roman", size = 20),
          #legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background =  element_blank(), #element_rect(fill = 'gray96'),
          #axis.text.y = element_text(angle = 90, hjust = 0.5)
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  return(pMap)
  
}

################################################################
### Map of Annual mean temperature
################################################################
### BIOCLIM temperature is actual values x 10 °C
d2$bioclim1 <- d2$bioclim1 * 0.1

### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[1]]), values(lgm.mainland[[1]])* 0.1))
colnames(rasd)[3] <- "bioclim1"

# vector of colour anmes c(low.colour, high.colour) 
cols = c("blue", "white", "red")

# Set the climate range
min(rasd$bioclim1, na.rm = T)
max(rasd$bioclim1, na.rm = T)

min(d2$bioclim1, na.rm = T)
max(d2$bioclim1, na.rm = T)

### Plot the LGM map
bi1_lgm <- map_plot(rasd, "bioclim1") + 
  # LGM NZ outline
  geom_polygon(data = lgmland, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient2("(°C)", low = cols[1], mid = cols[2], high = cols[3],
                       na.value = "white",
                       limits = c(-3, 17)
  )

### Plot current map
bi1_p <- map_plot(d2[, c("x", "y", "bioclim1")], "bioclim1") + 
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient2("(°C)", low = cols[1], mid = cols[2], high = cols[3],
                      na.value = "white",
                      limits = c(-3, 17)
  )


png(paste("Y://bio1.png", sep=""), width = 1200, height = 600)
grid.arrange(bi1_lgm, bi1_p, top = "Annual mean temperature", ncol = 2, widths = c(1,1))
dev.off()

################################################################
### Map of min temperature of the coldest month
################################################################
### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[2]]), values(lgm.mainland[[2]])* 0.1))
colnames(rasd)[3] <- "bioclim6"

### BIOCLIM temperature is actual values x 10 °C
d2$bioclim6 <- d2$bioclim6 * 0.1

# Set the climate range
min(rasd$bioclim6, na.rm = T)
max(rasd$bioclim6, na.rm = T)

min(d2$bioclim6, na.rm = T)
max(d2$bioclim6, na.rm = T)

# vector of colour anmes c(low.colour, high.colour) 
cols = c("blue", "white", "red")

### Plot the LGM map
bi6_lgm <- map_plot(rasd, "bioclim6") + 
  # LGM NZ outline
  geom_polygon(data = lgmland, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient2("(°C)", low = cols[1], mid = cols[2], high = cols[3],
                       na.value = "white",
                       limits = c(-13, 10)
  )

### Plot current map
bi6_p <- map_plot(d2[, c("x", "y", "bioclim6")], "bioclim6") + 
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient2("(°C)", low = cols[1], mid = cols[2], high = cols[3],
                       na.value = "white",
                       limits = c(-13, 10)
  )


png(paste("Y://bio6.png", sep=""), width = 1200, height = 600)
grid.arrange(bi6_lgm, bi6_p, top = "Minimum temperature in the coldest month", ncol = 2, widths = c(1,1))
dev.off()

################################################################
### Map of Annual precipitation
################################################################

### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[3]]), values(lgm.mainland[[3]])* 0.1))
colnames(rasd)[3] <- "bioclim12"

min(rasd$bioclim12, na.rm = T)
max(rasd$bioclim12, na.rm = T)

# vector of colour names c(low.colour, high.colour) 
cols = c("white", "blue")

# Plot the LGM map

bi12_lgm <- map_plot(rasd, "bioclim12") +
  # LGM NZ outline
  geom_polygon(data = lgmland, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient("(mm)", low = cols[1], high = cols[2],
                       na.value = "white",
                       limits = c(30, 6000)
  )

min(d2$bioclim12, na.rm = T)
max(d2$bioclim12, na.rm = T)

# Plot the current map
bi12_p <- map_plot(d2[, c("x", "y", "bioclim12")], "bioclim12") +
  
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient("(mm)", low = cols[1], high = cols[2],
                       na.value = "white",
                       limits = c(30, 6000)
  )

png(paste("Y://bio12.png", sep=""), width = 1200, height = 600)
grid.arrange(bi12_lgm, bi12_p, top = "Annual precipitation", ncol = 2, widths = c(1,1))
dev.off()

################################################################
### Map of Annual precipitation
################################################################

### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[4]]), values(lgm.mainland[[4]])))
colnames(rasd)[3] <- "bioclim15"

min(rasd$bioclim15, na.rm = T)
max(rasd$bioclim15, na.rm = T)

min(d2$bioclim15, na.rm = T)
max(d2$bioclim15, na.rm = T)

# vector of colour names c(low.colour, high.colour) 
cols = c("green", "red")

# Plot the LGM map

bi15_lgm <- map_plot(rasd, "bioclim15") +
  # LGM NZ outline
  geom_polygon(data = lgmland, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient("", low = cols[1], high = cols[2],
                      na.value = "white",
                      limits = c(7, 36)
  )

# Plot the current map
bi15_p <- map_plot(d2[, c("x", "y", "bioclim15")], "bioclim15") + 
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = NA) +
  
  scale_fill_gradient("", low = cols[1], high = cols[2],
                      na.value = "white",
                      limits = c(7, 36)
  )

png(paste("Y://bio15.png", sep=""), width = 1200, height = 600)
grid.arrange(bi15_lgm, bi15_p, top = "Precipitation seasonarity", ncol = 2, widths = c(1,1))
dev.off()