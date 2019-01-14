######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

# Load functions
source(".//SAI//F_modified_SpatialAvailabilityIndex.R")

######################################################################################
### Data import
######################################################################################

# LGM climate
# load(paste(".\\LGM_mainisland_worldclim1_5km_scores.data", sep = ""))
# # SAI for cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
# Current climate
load(".\\Scores_Acaena_landcover5km.data")

# Get rid of unnneccesary columns
scores <- scores[, -grep("^Acaena", colnames(scores))]

######################################################################################
### Calculate Spatial availability index of current climnate at the present
######################################################################################

# Get radius sizes
coordinateNames <- c("PC1","PC2")
ranges <- lapply(coordinateNames, get_radius_size, dat = scores)
names(ranges) <- coordinateNames

dat2 = scores

sai <- list()
for(i in 1:nrow(scores)){
  
  p <- scores[i, ]
  
  ######################################################################################
  # Prepare a x a km2 neighbourhood window
  ######################################################################################
  ### For the first step, try a = 20; window size = 20 x 20 km2
  # Unit of NZTM is meter, so 10000 m = 10km = 20km/2
  # Crop data by longitude
  dat.x <- Count_cells_within_neighbourhood(p, scores.lgm, 10000, "x")
  # By latitude
  neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, 10000, "y")
  
  ### SAI calculation
  sai[i] <- SAI(p, # a point at the centre of search area
                neighbour.window, # data of points to be searched
                ranges, # result of get_radius_size()
                coordinateNames # column name for climate variable in p and dat2
  )
}


save(sai, file = "SAI_5km_LGM_PC1_2.data")


