###################################################################################################
### Diagram showing how to calculate Spatial Availabiilty Index
###################################################################################################

source(".\\SAI\\F_SpatialAvailabilityIndex.R")
library(rgdal)
library(raster)
library(ggplot2)

###################################################################################################
### Calculate SAI and draw AUC figure
###################################################################################################

## Load 5km data
load(".\\Scores_Acaena_landcover5km.data")
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

### Select the target point to draw figures
n = 10000 
p <- scores[n, ]

# Get climate ranges of cells within the neighbourhood window 
ranges <- lapply(coordinateNames, get_radius_size, dat = scores)
names(ranges) <- coordinateNames

### SAI calculation
neighbours.size <- cells_within_neighbourhood_multivariate(p, # a point at the centre of search area
                                                           scores, # data of points to be searched
                                                           ranges, # result of get_radius_size()
                                                           twicerange = T,
                                                           coordinateNames # column name for climate variable in p and dat2
)

# Calculate percentage of area within the neighbourhood over NZ
ratio <- lapply(2:length(ranges[[1]]),
                function(j){
                  # Find points of a group within neighbourhood of another group of points 
                  nrow(neighbours.size[[j]]) / nrow(scores)
                }
)

### Calculate AUC (area under curve) for each current grid cells
res <- auc(c(0, 1:10)*0.1, c(0, ratio), type = "spline")

###################################################################################################
### Plot the AUC figure
###################################################################################################

png("AUC.png")

plot(seq(0,100,10), c(0, unlist(ratio))*100,
     xlab = "Proportion of climate ranges (%)",
     ylab = "Proportion of analogous climates (%)"
)
lines(seq(0,100,10), c(0, unlist(ratio))*100)
text(80,20, paste("Area Under Curve =", round(res,2)))

dev.off()
###################################################################################################
### Calculate climate similarity as the % of climate range within which the neighbourhood cell is
###################################################################################################


scores$ID <- 1:nrow(scores)

neighbours.size <- cells_within_neighbourhood_multivariate(p, # a point at the centre of search area
                                                           scores[,c(coordinateNames,"x","y","ID")], # data of points to be searched
                                                           ranges, # result of get_radius_size()
                                                           twicerange = T,
                                                           coordinateNames # column name for climate variable in p and dat2
)

scores.neigh <- scores

for(i in 2:10){
  
  scores.neigh [, paste("neigh", i)] <- NA
  scores.neigh [, paste("neigh", i)] <- sapply(1:nrow(scores.neigh), function(j){
    ifelse(sum(scores.neigh$ID[j] == neighbours.size[[i]]$ID) > 0, i, 0)
  }
  )
}


scores.neigh$similarity <- sapply(1:nrow(scores.neigh), function(i){
  ifelse(scores.neigh[i, "neigh 2"] > 0, 10,
            ifelse( scores.neigh[i, "neigh 3"] > 0, 20,
                      ifelse( scores.neigh[i, "neigh 4"] > 0, 30,
                                ifelse( scores.neigh[i, "neigh 5"] > 0, 40,
                                          ifelse( scores.neigh[i, "neigh 6"] > 0, 50,
                                                    ifelse( scores.neigh[i, "neigh 7"] > 0, 60,
                                                              ifelse( scores.neigh[i, "neigh 8"] > 0, 70,
                                                                        ifelse( scores.neigh[i, "neigh 9"] > 0, 80, 
                                                                                ifelse( scores.neigh[i, "neigh 9"] > 0, 90,100
                                                                                )))))))))
  }
)



###################################################################################################
### Extract elevation for 5km grid resolution raster
###################################################################################################

# Reference rasters
ref <- raster("Y://GIS map and Climate data//current_landcover5km.bil")
# create new raster having the same dimentions as reference raster (ex. pre-human map)
rast <- raster(ncol = ncol(ref), nrow = nrow(ref))
extent(rast) <- extent(ref)

pts <- scores.neigh[, c("x", "y")]

# point coordinate system setting
coordinates(pts) <- scores.neigh[, c("x", "y")]
proj4pts <- proj4string(ref)
proj4string(pts) <- CRS(proj4pts)
# land use change column
pts$similarity <- scores.neigh$similarity

# rasterize
prast <- rasterize(pts, ref, field = pts$similarity, fun = mean)

###################################################################################################
### Plot the point on map
###################################################################################################
p <- scores.neigh[n, ]
#### Set colour gradient
colfunc <- colorRampPalette(c("red", "yellow"))

png("example_map.png")
plot(prast, col = colfunc(10))
points(p[, c("x","y")], pch=15)
dev.off()
###################################################################################################
### Plot the point on climate space
###################################################################################################

scores.neigh$bioclim1 <- scores.neigh$bioclim1/10

climatespace <- ggplot(data= scores.neigh) +
  geom_point(aes(x= bioclim1, y = bioclim12, color = similarity)) +
  geom_point(data = scores.neigh[i,], aes(x= bioclim1, y = bioclim12), color="black") +
  scale_colour_gradientn(colours = colfunc(10)) +
     xlab("Annual mean temperature (\u00B0C)")+ 
     ylab("Annual precipitation (mm)") +
  theme_classic()

ggsave("example_climate.png", plot = climatespace, width = 15, height = 10, units = "cm")

