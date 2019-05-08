###################################################################################################
### Calculate SAI
###################################################################################################

source(".\\SAI\\F_SpatialAvailabilityIndex.R")

###################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

SAI_for_limited_ranges <- function(time, # "current" or "LGM"
                         neighbour.window.size, # Size of windows to calculate the SAI (km)
                         standardize, # Standardize cliamte data
                         whole # TRUE; neighbourhood window = whole NZ
){
  
  # Change object name of data frame with 
  if(time == "current"){  
    ### Load 5km data
    load(".\\Scores_Acaena_landcover5km.data")
    coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")
  }else{
    # data of LGM mainland
    load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
    scores <- scores.lgm
    coordinateNames <- c("bi1", "bi6", "bi12", "bi15")
    
  }
  
  if(standardize == TRUE){
    scores <- cbind(scores[, !(colnames(scores) %in% coordinateNames)], scale(scores[, coordinateNames]))
  }else{
    scores <- scores
  }
  

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

###### Current

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i<- SAI_for_limited_ranges("current", i, standardize = FALSE, whole=F)
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_", i,"kmWindow_4var_climateRange_of_neighbourhood.data", sep=""))
  
}

# Whole NZ
sai.i<- SAI_for_limited_ranges("current", 5000, standardize = FALSE, whole=T)
# Save
save(sai.i, file = "SAI_5km_currentInCurrent_5000kmWindow_4var.data")

###### LGM

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i<- SAI_for_limited_ranges("LGM", i, standardize = FALSE, whole=F)
  # Save
  save(sai.i, file = paste("SAI_5km_LGMInLGM_", i,"kmWindow_4var.data", sep=""))
  
}

# Whole NZ
sai.i<- SAI_for_limited_ranges("LGM", 5000, standardize = FALSE, whole=T)
# Save
save(sai.i, file = "SAI_5km_LGMInLGM_5000kmWindow_4var.data")




### Standardized SAI
for(i in c(20,500)){
  ### 1500 km neighbourhood window = whole NZ
  sai.i <- SAI_for_limited_ranges("current", i, standardize = TRUE, whole=F)
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_",i,"kmWindow_4var_standardized.data", sep=""))
}
