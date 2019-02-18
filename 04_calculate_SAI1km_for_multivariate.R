###################################################################################################
### Calculate SAI
###################################################################################################

source(".\\SAI\\F_SpatialAvailabilityIndex.R")

### Load 1km data
scores <- read.csv("D:\\PhD\\current_south_island_climate1km.csv")
# ### Load 5km data
#load(".\\Scores_Acaena_landcover5km.data")

coordinateNames <- c("bioclim10", "bioclim11", "bioclim18", "bioclim19")
ranges <- lapply(coordinateNames, get_radius_size, dat = scores)
names(ranges) <- coordinateNames

dat2 = scores

sai <- list()
for(i in 1:nrow(scores)){
  
  p <- scores[i, ]
  
  ######################################################################################
  # Prepare a x a km2 neighbourhood window
  ######################################################################################
  # For the first step, try a = 20
  # Unit of NZTM is meter, so 10000 m = 10km = 20km/2
  dat.x <- Count_cells_within_neighbourhood(p, dat2, 10000, "x")
  neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, 10000, "y")
  
  ### SAI calculation
  sai[i] <- SAI(p, # a point at the centre of search area
                neighbour.window, # data of points to be searched
                ranges, # result of get_radius_size()
                coordinateNames # column name for climate variable in p and dat2
)
}

# Load SAI values
save(sai, file = "SAI_south_1km_current_4var.data")
