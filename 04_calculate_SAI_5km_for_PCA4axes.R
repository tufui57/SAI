############################################################################################################
## SAI based PCA values
############################################################################################################

source(".\\SAI\\F_SpatialAvailabilityIndex.R")

###################################################################################################
### Calculate PCA scores
###################################################################################################

# data frame of occurrence data and climate data
datapath <- "Y://5th chapter SAI chapter//raw data//Acaena_bioclim_landcover_history_worldclim1_5km.csv"
d<- read.csv(datapath)

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 11, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], "x", "y")], pca$x[, 1:4])

# Save data
save(scores, file = ".\\Scores_5km_PCA_4axes.data")

###################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

SAI_for_limited_ranges <- function(time, # "current" or "LGM"
                                   neighbour.window.size, # Size of windows to calculate the SAI (km)
                                   whole # TRUE; neighbourhood window = whole NZ
){
  
  # Change object name of data frame with 
  coordinateNames <- paste("PC", 1:4, sep="")
  
  sai <- list()
  
  for(i in 1:nrow(scores)){
    
    # Set a point at the centre of search area
    p <- scores[i, ]
    
    if(whole == F){
      ######################################################################################
      # Prepare a x a km2 neighbourhood window as an area to be searched
      ######################################################################################
      # For the first step, try a = 20
      # Unit of NZTM is meter, so 10000 m = 10km = 20km/2
      a <- neighbour.window.size * 1000 / 2
      dat.x <- Count_cells_within_neighbourhood(p, scores, a, "x")
      neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, a, "y")
      
    }else{
      neighbour.window <- scores
      
    }
    
    # Get climate ranges of cells within the neighbourhood window 
    ranges <- lapply(coordinateNames, get_radius_size, dat = neighbour.window)
    names(ranges) <- coordinateNames
    
    
    ### SAI calculation
    sai[i] <- SAI(p, # a point at the centre of search area
                  neighbour.window, # data of points to be searched
                  ranges, # result of get_radius_size()
                  twicerange = TRUE,
                  coordinateNames # column name for climate variable
    )
  }
  
  return(sai)
}

# Whole NZ
sai.i<- SAI_for_limited_ranges("current", 5000, whole=T)
# Save
save(sai.i, file = "SAI_5km_currentInCurrent_5000kmWindow_PCA4axes.data")
