########################################################################################
### Elevation map
########################################################################################

library(raster)
# Reference raster for 5km grid resolution
pre <- raster("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil")

# Load elevation raster
elev <- raster("Y:\\GIS map and Climate data\\kx-nz-80m-digital-elevation-model-GTiff\\nztm.tif")
elev.wgs <- projectRaster(elev, pre)

elev.dat <- data.frame(coordinates(elev.wgs), values(elev.wgs))
colnames(elev.dat)[3] <- "elev"

elev.dat[elev.dat$elev == 0 & is.na(elev.dat$elev) == FALSE, "elev"] <- NA

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
myplot <- ggplot(elev.dat, aes_string("x", "y", fill = "elev")) + 
  geom_raster() +
  scale_fill_gradientn(colours = terrain.colors(300), 
                       na.value = "transparent") +
  geom_polygon(data=nzland2,aes(x=long,y=lat,group=group), fill=NA, col="black") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=15)
  )

ggsave(plot = myplot, filename = "Y:\\elevation_map.png", 
       width = 16, height = 20, units = "cm"
       )
