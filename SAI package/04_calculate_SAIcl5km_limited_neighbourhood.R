#######################################################################################################
### Calculate spatial availability index of current climates under the curretn conditions
#######################################################################################################

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")

######################################################################################
### Data import
######################################################################################

# the LGM climate scores
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
# Current climate scores
load(".\\Scores_Acaena_landcover5km.data")

###############################################################################################
### Function to calculate Spatial availability index of current climnate at the LGM (SAIcl)
###############################################################################################

SAIcl <- function(
  data1, # data.frame containing data of grid cells to calcualte SAI for.
  data2, # data.frame containing data of grid cells to search analogous climates to cells in data1.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size, # Size of neighbourhood area to search analogous climates (km)
  no.neighbourhood # True/False; T = no limit for neighbourhood to search analogous climates, F = give neighbourhood.size
){
  
  sai <- list()
  
  for(i in 1:nrow(data1)){
    
    #  p must have variable values of the current
    p <- data1[i, ]
    
    if(no.neighbourhood == F){
      ######################################################################################
      ## Identify the neighbourhood to calcualte SAI within a km neighbourhood
      ######################################################################################
      
      # Prepare a x a km2 square as the neighbourhood to search analogous climates.
      # Identify grid cells of LGM data within the neighbourhood
      a <- neighbourhood.size * 1000 / 2
      dat.x <- Count_cells_within_neighbourhood(p, data2, a, coordinateNames[1])
      neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, a, coordinateNames[2])
      
      # Get current cells within the neighbourhood
      dat.x <- Count_cells_within_neighbourhood(p, data1, a, coordinateNames[1])
      neighbour.window.current <- Count_cells_within_neighbourhood(p, dat.x, a, coordinateNames[2])
    
      # Calcualte steps of climate range breaths for the grid cells within the neighbourhood
    scores.int <- rbind(neighbour.window[,c(coordinateNames, climateNames)], neighbour.window.current[,c(coordinateNames, climateNames)])
    ranges.int <- lapply(climateNames, get_radius_size, dat = scores.int)
    names(ranges.int) <- climateNames
    
    }else{
      ######################################################################################
      ## Identify the neighbourhood to calcualte SAI within NZ
      ######################################################################################
      
      neighbour.window <- data2
      # Calcualte steps of climate range breaths for the grid cells within the neighbourhood
      ranges.int <- lapply(climateNames, get_radius_size, dat = neighbour.window[,c(coordinateNames, climateNames)])
      names(ranges.int) <- climateNames
    }
    

    
    ### SAI calculation
    sai[i] <- SAI(p, # a point at the centre of search area.
                  neighbour.window, # data of points to be searched. 
                  ranges.int, # result of get_radius_size()
                  twicerange = TRUE,
                  climateNames # column names for climate variables in p and neighbour.window
    )
  }
  
  return(sai)

}

######################################################################################
### Calculate SAIcl
######################################################################################


# Column names of environmental variables to search for analogous environment conditions
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")
# Colnames for target variables of current and LGM scores must be shared.
colnames(scores.lgm) <- gsub("bi", "bioclim", colnames(scores.lgm))

coordinateNames = c("x", "y")

# SAIcl within 20 km neighbourhood areas 
SAIcl(
    data1 = scores, # data.frame containing data of grid cells to calcualte SAI for.
    data2 = scores.lgm, # data.frame containing data of grid cells to search analogous climates to cells in data1.
    climateNames = climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
    coordinateNames = coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
    climateRangeStep = 10, # Integer. Breaks of cliamte range breaths(%)
    neighbourhood.size = 20, # Size of neighbourhood area to search analogous climates (km)
    no.neighbourhood = F  # True/False; T = no limit for neighbourhood to search analogous climates, F = give neighbourhood.size
  )


# SAIcl for NZ
sai.i <- SAIcl(scores, scores.lgm, climateNames, coordinateNames,climateRangeStep = 10,
  neighbourhood.size = 5000,  no.neighbourhood = T)
# Save
save(sai.i, file = "SAI_5km_currentInLGM_5000kmWindow_4var.data")
