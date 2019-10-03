########################################
### Map Switzerland
########################################

library(dplyr)
library(rgdal)
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)
source(".\\functions\\F04_convert_Points_to_raster.R")

########################################
### Data preparation
########################################

swiss <- read.csv("Y://swiss_all_climate_2.csv")

# Region 1
p = swiss[(swiss$LONG <= 7.65) & (swiss$LONG >= 7.4),]
reg1 <- p[(p$LAT <= 47.3) & (p$LAT >= 47),]

# Region 2
p = swiss[(swiss$LONG <= 7.4) & (swiss$LONG >= 7.15),]
reg2 <- p[(p$LAT <= 46.55) & (p$LAT >= 46.2),]

# Region 3
p = swiss[(swiss$LONG <= 8.9) & (swiss$LONG >= 8.65),]
reg3 <- p[(p$LAT <= 46.75) & (p$LAT >= 46.45),]

################################################################################
### Find the map resolution from dataframe
################################################################################

res <- list()
for(i in 1:length(reg1$LAT)){
  res[i] <- reg1$LAT[i] - reg1$LAT[i+1]
}

unlist(res)[unlist(res)!=0] %>% unique

################################################################################
### Fix the map resolution
################################################################################

# If the intervals of coordinates in a dataframe are uneven, white lines will appear in the map plots. 
# To fix this, an empty raster with correct coordinates was generated, and the dataframe was resampled on the raster.

# Rasterize elevation data
res.swiss <- unevenly_gridded_dataframe_to_raster(swiss, "ELEV", # colum name of raster values
                                     c("LONG", "LAT"), # column names of coodinates
                                     "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", # CRS
                                     0.00277 # resolution of raster
                                    )



## Overlay Swiss border and squares of example regions

# Swiss border data
path = "Y:\\swiss\\CHE_adm0.shp"
LAYERS <- ogrListLayers(path)
border <- readOGR(path, LAYERS)


### Rasterize cliamte data
tave68.s <- convert_dataframe_to_raster(swiss, res.swiss, "tave68", c("LONG", "LAT"))
tave122.s <- convert_dataframe_to_raster(swiss, res.swiss, "tave122", c("LONG", "LAT"))
prec49.s <- convert_dataframe_to_raster(swiss, res.swiss, "prec49", c("LONG", "LAT"))
prec103.s <- convert_dataframe_to_raster(swiss, res.swiss, "prec103", c("LONG", "LAT"))

rasters.s <- list(res.swiss, tave68.s, tave122.s,prec49.s,prec103.s)

# Clip the Swiss elevation data by the polygon of the border
source(".\\functions\\F_clip_raster_by_polygon.R")
clipped.swiss2 <- lapply(rasters.s, clip.by.polygon, shape = border)
names(clipped.swiss2) <- c("Elevation", "tave68", "tave122", "prec49","prec103")

save(clipped.swiss2, file = "Y://swiss_climate_clipped.data")

################################################################################
### Map
################################################################################

# Add squares showing the example regions on the map
d <- data.frame(x1 = c(7.4,7.15,8.65), x2 = c(7.65,7.4,8.9), y1 = c(47,46.225,46.45), y2 = c(47.3,46.525,46.75))
d2 <- data.frame(cbind(d$x1 - 0.2, d$x2 + 0.2, d$y1 - 0.2, d$y2 + 0.2))
colnames(d2) <- colnames(d)

# Map SAI
png("Y:\\swiss_map.png", width = 1300, height = 630)

ggplot(res.swiss3, aes_string("Longitude", "Latitude", fill = "Elevation")) + 
  geom_raster() +
  scale_fill_gradientn( colors=c('#a6611a','#dfc27d','#f5f5f5','#80cdc1','#018571')) +
  geom_rect(data = d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
            alpha = 0, inherit.aes = FALSE, size = 1) +
  geom_rect(data = d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
            alpha = 0, inherit.aes = FALSE, linetype = 2, size = 1) +
  geom_polygon(data = border,aes(x = long, y = lat, group = group), fill = NA, col = "black") +
  theme(panel.background = element_blank(),
        panel.border= element_rect(color="black", fill = NA),
        text = element_text(size=25)
  )
dev.off()




################################################################################
### Fix the resolutions of example regions
################################################################################

################################################################################
### Region 1
################################################################################

load(".//SAIcc_swissRegion1_25km_neighbourhood_ref.data")
swiss.reg1 <- cbind(reg1, unlist(sai.swiss))
colnames(swiss.reg1)[ncol(swiss.reg1)] <- "EP"


# Rasterize data
reg1.tes <- unevenly_gridded_dataframe_to_raster(swiss.reg1, 
                                                 "EP", # colum name of raster values
                                              c("LONG", "LAT"), # column names of coodinates
                                              "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", # CRS
                                              0.00275 # resolution of raster
)

png("Y://EP.swissRegion1.png", width = 900, height = 630)
plot_SAI(scores = data.frame(coordinates(reg1.tes)), sai = sai.swiss, nameOfsai = "EPcc", coordinateNames = c("x", "y"), colfunc)
dev.off()

################################################################################
### Region 2
################################################################################


load(".//SAIcc_swissRegion2_25km_neighbourhood_ref.data")
swiss.reg1 <- cbind(reg2, unlist(sai.swiss2))
colnames(swiss.reg1)[ncol(swiss.reg1)] <- "EP"


# Rasterize data
reg2.tes <- unevenly_gridded_dataframe_to_raster(swiss.reg1, 
                                                 "EP", # colum name of raster values
                                                 c("LONG", "LAT"), # column names of coodinates
                                                 "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", # CRS
                                                 0.00275 # resolution of raster
)

png("Y://EP.swissRegion2.png", width = 900, height = 630)
plot_SAI(scores = data.frame(coordinates(reg2.tes)), sai = sai.swiss2, nameOfsai = "EPcc", coordinateNames = c("x", "y"), colfunc)
dev.off()

################################################################################
#### Region 3
################################################################################


load(".//SAIcc_swissRegion3_25km_neighbourhood_ref.data")
swiss.reg1 <- cbind(reg3, unlist(sai.swiss3))
colnames(swiss.reg1)[ncol(swiss.reg1)] <- "EP"


# Rasterize data
reg3.tes <- unevenly_gridded_dataframe_to_raster(swiss.reg1, 
                                                 "EP", # colum name of raster values
                                                 c("LONG", "LAT"), # column names of coodinates
                                                 "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", # CRS
                                                 0.00275 # resolution of raster
)

png("Y://EP.swissRegion3.png", width = 900, height = 630)
plot_SAI(scores = data.frame(coordinates(reg3.tes)), sai = sai.swiss3, nameOfsai = "EPcc", coordinateNames = c("x", "y"), colfunc)
dev.off()

