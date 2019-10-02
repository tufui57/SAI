########################################
### Map the world
########################################

library(dplyr)
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)
source(".\\functions\\F04_convert_Points_to_raster.R")
source(".\\EP\\functions_EnvironmentalPrevalenceIndex.R")

########################################
### Data preparation
########################################
files <- list.files("Y:\\Writings\\Thesis\\3rd chapter\\Publication\\Climate data")
files2 <- paste("Y:\\Writings\\Thesis\\3rd chapter\\Publication\\Climate data\\", files[grepl("^05", files)], sep = "")

wor <- lapply(files2, read.table, header = T)

wor1 <- cbind(sapply(wor, function(x){return(x[,4])}))

wor2 <- cbind(wor[[1]][, c("x","y")], wor1)
colnames(wor2) <- c("x","y", paste("bio", c(1,6,12,15), sep=""))


climateNames = paste("bio", c(1,6,12,15), sep="")

ep.world <- calc_EP(data1 = wor2,
                      data2 = wor2,
                      climateNames = climateNames,
                      coordinateNames = c("x","y")
)
# Save
save(ep.world, file = "EPcc_world.data")

