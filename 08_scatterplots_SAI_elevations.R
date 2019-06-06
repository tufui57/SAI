############################################################################################################
## SAI vs. elevations
############################################################################################################

library(dplyr)
library(ggplot2)
library(rgdal)
library(raster)
library(gridExtra)

### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(20,50,100))){
  a <- load(paste("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_",  i, "kmWindow_4var_climateRange_of_neighbourhood.data", sep=""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}

a <- load("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_5000kmWindow_4var.data")
a <- get(a)
sai.c[[4]] <- unlist(a)

sai.c2 <- data.frame(do.call(cbind,sai.c))
colnames(sai.c2)[4] <- "NZ"

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

load("Y://Acaena_bioclim_landcover_history_worldclim1_5km.csv")

dat <- cbind(scores[, c("x","y")], sai.c2)

########################################################################################
### Elevation of any grid cells in NZ
########################################################################################

library(raster)
# Reference raster for 5km grid resolution
pre <- raster("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil")

# Load elevation raster
elev <- raster("Y:\\GIS map and Climate data\\kx-nz-80m-digital-elevation-model-GTiff\\nztm.tif")
elev.wgs <- projectRaster(elev, pre)

elev.dat <- data.frame(coordinates(elev.wgs), values(elev.wgs))
colnames(elev.dat)[3] <- "elev"

dat2 <- merge(dat, elev.dat, by=c("x","y"))


########################################################################################
### Linear regression between elevation and SAI
########################################################################################

summary(lm(dat2$X20 ~ dat2$elev))

source(".//functions//F_plotAnalysis_clade_niche.R")
# Plot
myplot20 <- plotAnalysis(data = dat2, 
                       yv = "X20", xv = "elev",
                       showStats = T,
                       ylabname = "SAI within 20 km neighbourhood", 
                       xlabname = "Elevation (m)",
                       label.point = F,
                       genus_name = ""
) +
  ylim(0, 1)

myplot50 <- plotAnalysis(data = dat2, 
                          yv = "X50", xv = "elev",
                          showStats = T,
                          ylabname = "SAI within 50 km neighbourhood", 
                          xlabname = "Elevation (m)",
                          label.point = F,
                          genus_name = ""
) +
  ylim(0, 1)

summary(lm(dat2$NZ ~ dat2$elev))
myplot100 <- plotAnalysis(data = dat2, 
                         yv = "X100", xv = "elev",
                         showStats = T,
                         ylabname = "SAI within 100 km neighbourhood", 
                         xlabname = "Elevation (m)",
                         label.point = F,
                         genus_name = ""
) +
  ylim(0, 1)

myplotNZ <- plotAnalysis(data = dat2, 
                          yv = "NZ", xv = "elev",
                          showStats = T,
                          ylabname = "SAI within New Zealand", 
                          xlabname = "Elevation (m)",
                          label.point = F,
                          genus_name = ""
) +
  ylim(0, 1)


png("Y:\\SAI_elevation.png", width = 600, height = 600)

# Plot in multiple panels
grid.arrange(
  arrangeGrob(myplot20, myplot50, myplot100, myplotNZ,
              ncol = 2, nrow = 2)
)
dev.off()

########################################################################################
### Compare means of SAI between high and low elevational areas
########################################################################################
median(dat2$NZ)
median(dat2[dat2$elev > 1500, "NZ"])
median(dat2[dat2$elev < 500, "NZ"])

median(dat2[dat2$elev > 1500, "X20"])
median(dat2[dat2$elev < 500, "X20"])


