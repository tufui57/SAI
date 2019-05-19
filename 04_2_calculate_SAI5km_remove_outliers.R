
source(".\\SAI\\F_SpatialAvailabilityIndex.R")


###################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

SAI_outliers <- function(time, # "current" or "LGM"
                         outlierPercent # how many percentage of outliers should be removed from the varibale widths
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
    ranges_without_outliers(coordinateName = i, outlierPercent, dat = scores)
  }
  )
  names(ranges) <- coordinateNames
  
  sai <- list()
  
  for(i in 1:nrow(scores)){
    
    # Set a point at the centre of search area
    p <- scores[i, ]
    
    neighbour.window <- scores

    
    ### If any values of variables of p is outliers, the values must be replaced with min or max values of the variables.
    res<-list()
    for(j in coordinateNames){
      res[[j]] <- check_outliers(p, scores, j, outlierPercent)
    }
    
    # Replace the values
    p.cor <- p
    p.cor[,coordinateNames] <- unlist(res)
    
    ### SAI calculation
    sai[i] <- SAI(p.cor, # a point at the centre of search area
                  neighbour.window, # data of points to be searched
                  ranges, # result of get_radius_size()
                  twicerange=TRUE,
                  coordinateNames # column name for climate variable
    )
  }
  
  return(sai)
}

###### Current

for(i in c(1, 2.5, 5)){
  # Whole NZ
sai.i <- SAI_outliers("current", outlierPercent = i)
# Save
save(sai.i, file = paste("SAI_5km_currentInCurrent_5000kmWindow_4var_outlier", i, ".data", sep=""))

}

###### LGM

for(i in c(1, 2.5, 5)){
  # Whole NZ
  sai.i <- SAI_outliers("LGM", outlierPercent = i)
  # Save
  save(sai.i, file = paste("SAI_5km_LGMinLGM_5000kmWindow_4var_outlier", i, ".data", sep=""))
  
}

