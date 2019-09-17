###################################################################################################
### Calculate SAI
###################################################################################################

source(".\\SAI\\F_SpatialAvailabilityIndex.R")


##################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

SAIcc_ll <- function(
  data1, # data.frame containing data of grid cells to calcualte SAI for, and search analogous climates.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size, # Size of neighbourhood area to search analogous climates (km)
  no.neighbourhood, # True/False; T = no limit for neighbourhood to search analogous climates, F = give neighbourhood.size
  standardize = F, # Standardize cliamte data
){
  

  
  if(standardize == TRUE){
    data1 <- cbind(data1[, !(colnames(data1) %in% climateNames)], scale(data1[, climateNames]))
  }else{
    data1 <- data1
  }
  

  sai <- list()
  
  for(i in 1:nrow(data1)){
    
    # Set a point at the centre of search area
    p <- data1[i, ]
    
    if(no.neighbourhood == F){
      ######################################################################################
      # Prepare a x a km2 neighbourhood window as an area to be searched
      ######################################################################################
      # For the first step, try a = 20
      # Unit of NZTM is meter, so 10000 m = 10km = 20km/2
      a <- neighbourhood.size * 1000 / 2
      dat.x <- Count_cells_within_neighbourhood(p, data1, a, coordinateNames[1])
      neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, a, coordinateNames[2])
      
    }else{
      neighbour.window <- data1
      
    }
    
    # Get climate ranges of cells within the neighbourhood window 
    ranges <- lapply(climateNames, get_radius_size, dat = neighbour.window)
    names(ranges) <- climateNames
    
      
    ### SAI calculation
    sai[i] <- SAI(p, # a point at the centre of search area
                  neighbour.window, # data of points to be searched
                  ranges, # result of get_radius_size()
                  twicerange = TRUE,
                  climateNames # column name for climate variable
    )
  }
  
  return(sai)
}



#############################################
###### Current
#############################################

### Load current cliamte data at 5km resolution
load(".\\Scores_Acaena_landcover5km.data")
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")


for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i <- SAIcc_ll(scores, 
                   climateNames,
                   coordinateNames = c("x","y"),
                   neighbourhood.size = i, 
                   no.neighbourhood = F
                   )
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_", i,"kmNeighbourhood.data", sep=""))
  
}

# Whole NZ
sai.i <- SAIcc_ll(scores, 
                 climateNames,
                 coordinateNames = c("x","y"),
                 neighbourhood.size = 5000, 
                 no.neighbourhood = T
                 )
# Save
save(sai.i, file = "SAI_5km_currentInCurrent_5000kmWindow_4var.data")

#############################################
###### LGM
#############################################

# data of LGM mainland
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
scores <- scores.lgm
climateNames <- c("bi1", "bi6", "bi12", "bi15")

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i <- SAIcc_ll(scores, 
                    climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i, 
                    no.neighbourhood = F
  )
  # Save
  save(sai.i, file = paste("SAI_5km_LGMInLGM_", i,"kmNeighbourhood.data", sep=""))
  
}

# Whole NZ
sai.i <- SAIcc_ll(scores, 
                  climateNames,
                  coordinateNames = c("x","y"),
                  neighbourhood.size = 5000, 
                  no.neighbourhood = T
)
# Save
save(sai.i, file = "SAI_5km_LGMInLGM_5000kmWindow_4var.data")


#############################################
### SAI for standardized climate values 
#############################################

for(i in c(20,5000)){
  ### i km neighbourhood window
  sai.i <- SAIcc_ll(scores, 
                    climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i, 
                    no.neighbourhood = F,
                    standardize = T
  )
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_",i,"kmWindow_4var_standardized.data", sep=""))
}
