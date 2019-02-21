
source(".\\SAI\\F_SpatialAvailabilityIndex.R")


###################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

SAI_outliers <- function(time, # "current" or "LGM"
                         neighbour.window.size, # Size of windows to calculate the SAI (km)
                         outlierPercent, # how many percentage of outliers should be removed from the varibale widths
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
  
  # Set ranges of each variable
  ranges <- lapply(coordinateNames, function(i){
    ranges_without_outliers(coordinateName = i, outlierPercent = 1, dat = scores)
  }
  )
  names(ranges) <- coordinateNames
  
  sai <- list()
  
  for(i in 1:nrow(scores)){
    
    # Set a point at the centre of search area
    p <- scores[i, ]
    
    if(whole == F){
      ######################################################################################
      # Prepare a x a km2 neighbourhood window as an area to be searched
      ######################################################################################
      a <- neighbour.window.size * 1000 / 2
      dat.x <- Count_cells_within_neighbourhood(p, scores, a, "x")
      neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, a, "y")
      
    }else{
      neighbour.window <- scores
      
    }
    
    
    ### If any values of variables of p is outliers, the values must be replaced with min or max values of the variables.
    res<-list()
    for(j in coordinateNames){
      res[[j]] <- check_outliers(p, scores, j, 1)
    }
    
    # Replace the values
    p.cor <- p
    p.cor[,coordinateNames] <- unlist(res)
    
    ### SAI calculation
    sai[i] <- SAI(p.cor, # a point at the centre of search area
                  neighbour.window, # data of points to be searched
                  ranges, # result of get_radius_size()
                  coordinateNames # column name for climate variable
    )
  }
  
  return(sai)
}

###### Current

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i <- SAI_outliers("current", i, outlierPercent = 1, whole=F)
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_", i,"kmWindow_4var_outlier.data", sep=""))
  
}

# Whole NZ
sai.i <- SAI_outliers("current", 5000, outlierPercent = 1, whole=T)
# Save
save(sai.i, file = "SAI_5km_currentInCurrent_5000kmWindow_4var.data")

###### LGM

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i<- SAI_outliers("LGM", i, outlierPercent = 1, whole=F)
  # Save
  save(sai.i, file = paste("SAI_5km_LGMInLGM_", i,"kmWindow_4var.data", sep=""))
  
}

# Whole NZ
sai.i<- SAI_outliers("LGM", 5000, outlierPercent = 1, whole=T)
# Save
save(sai.i, file = "SAI_5km_LGMInLGM_5000kmWindow_4var.data")

