########################################
### Map Switzerland
########################################

library(dplyr)
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

# Rasterize data
res.swiss <- unevenly_gridded_dataframe_to_raster(swiss, "ELEV", # colum name of raster values
                                     c("LONG", "LAT"), # column names of coodinates
                                     "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", # CRS
                                    0.00277 # resolution of raster
)

res.swiss2 <- data.frame(cbind(coordinates(res.swiss), values(res.swiss)))



################################################################################
### Find the map resolution from dataframe
################################################################################

### Region 1
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


plot_SAI(scores = data.frame(coordinates(reg1.tes)), sai = sai.swiss, nameOfsai = "EPcc", coordinateNames = c("x", "y"), colfunc)


################################################################################
### Find the map resolution from dataframe
################################################################################

### Region 2
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


plot_SAI(scores = data.frame(coordinates(reg2.tes)), sai = sai.swiss2, nameOfsai = "EPcc", coordinateNames = c("x", "y"), colfunc)

################################################################################
### Find the map resolution from dataframe
################################################################################

### Region 3
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


plot_SAI(scores = data.frame(coordinates(reg3.tes)), sai = sai.swiss3, nameOfsai = "EPcc", coordinateNames = c("x", "y"), colfunc)
