###################################################################################################
### Calculate SAI
###################################################################################################

source(".\\SAI\\SAI package\\F_SpatialAvailabilityIndex.R")


##################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

calc_SAI <- function(
  data1, # data.frame containing data of grid cells to calcualte SAI for.
  data2, # data.frame containing data of grid cells to search analogous climates from.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size = NULL, # Size of neighbourhood area to search analogous climates (km)
  # Unit of NZTM is meter. This function might need an option for data whose coordinate unit is degree.
  standardize = F, # Standardize climate data
  ordination = F, # use of ordination axes instead of climate variables
  outlierRemoval = F, # remove climate outliers from data to search analogous climates from.
  outlierPercent = 0
){
  

  ########################################################################################### 
  # Check if the use of ordination axes instead of climate variables makes differnce in SAI.
  ###########################################################################################
  if(ordination == TRUE){
    # get env. corrdinates (PCA axes)
  pca <- prcomp(data1[, climateNames],
                center = TRUE,
                scale. = TRUE)
  data1 <- data.frame(data1[, c(climateNames, "x", "y")], pca$x[, 1:4])
  }
  
  ###################################################################################### 
  ### Check if standerdization makes differnce in SAI.
  ###################################################################################### 
  if(standardize == TRUE){
    data1 <- cbind(data1[, !(colnames(data1) %in% climateNames)], scale(data1[, climateNames]))
  }

  sai <- list()
  
  for(i in 1:nrow(data1)){
    
    # Target grid cell
    p <- data1[i, ]
    
    ######################################################################################
    # Prepare the neighbourhood square of the target cell
    ######################################################################################
    
    ## If data1 and data2 are the same 
    if(sum(data1[,3] != data2[,3]) == 0){
      
      neighbour.window <- data2

      if(is.null(neighbourhood.size)){

      }else{
        
        # Prepare a neighbourhood square to search analogous climate from.
        # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
        a <- neighbourhood.size * 1000 / 2
        dat.x <- analogousCells_in_single_variable(p, data2, a, coordinateNames[1])
        neighbour.window <- analogousCells_in_single_variable(p, dat.x, a, coordinateNames[2])
      }
      
      ranges <- lapply(climateNames, get_radius_size, dat = neighbour.window)
      names(ranges) <- climateNames
      
    }else{
      
      ## If data1!= data2
      if(is.null(neighbourhood.size)){
        
        # Combine cells of data1 and data2 within the neighbourhood
        neighbour.window.data1_2 <- rbind(neighbour.window[c(coordinateNames, climateNames)], 
                                          data1[c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_radius_size, dat = neighbour.window.data1_2)
        names(ranges) <- climateNames
        
      }else{
        
        # Prepare a neighbourhood square to search analogous climate from.
        # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
        a <- neighbourhood.size * 1000 / 2
        dat.x <- analogousCells_in_single_variable(p, data2, a, coordinateNames[1])
        neighbour.window <- analogousCells_in_single_variable(p, dat.x, a, coordinateNames[2])
        
        # Combine cells within the neighbourhood and the target point
        neighbour.window.data2_p <- rbind(neighbour.window[c(coordinateNames, climateNames)], 
                                          p[c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_radius_size, dat = neighbour.window.data2_p)
        names(ranges) <- climateNames
      }
      
      
    }

    
    ###################################################################################### 
    ### SAI calculation
    ###################################################################################### 
    sai[i] <- SAI(p, # a target grid cell
                  neighbour.window, # grid cells within the neighbourhood
                  ranges, # result of get_radius_size() or ranges_without_outliers()
                  climateNames # column names of climate variables
    )
    
    print(paste("finished the cell number", i))
  }
  
  return(sai)
}



#############################################
###### Current
#############################################

### Load current cliamte data at 5km resolution
load(".\\Scores_Acaena_landcover5km.data")
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

### SAI within i km neighbourhood
for(n in c(20,50,100)){
  
  sai.i <- calc_SAI(data1 = scores,
                    data2 = scores,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = n
                    )
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_", i,"kmNeighbourhood.data", sep=""))
  
}

### SAI within Whole NZ
sai.i <- calc_SAI(data1 = scores,
                  data2 = scores,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
                  )
save(sai.i, file = "SAIcc_NZ_4var.data")

###### Outlier removal

for(i in c(1, 2.5, 5)){
  # Whole NZ
  sai.i <- calc_SAI(data1 = scores,
                    data2 = scores,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    outlierRemoval = T,
                    outlierPercent = i
  )
  # Save
  save(sai.i, file = paste("SAIcc_NZ_outlier", i, ".data", sep=""))
  
}



#############################################
###### LGM
#############################################

# data of LGM mainland
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
climateNames <- c("bi1", "bi6", "bi12", "bi15")

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i <- calc_SAI(scores.lgm,
                    scores.lgm,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i
  )
  # Save
  save(sai.i, file = paste("SAI_5km_LGMInLGM_", i,"kmNeighbourhood.data", sep=""))
  
}

# Whole NZ
sai.i <- calc_SAI(scores.lgm, 
                  scores.lgm, 
                  climateNames,
                  coordinateNames = c("x","y")
                  )
# Save
save(sai.i, file = "SAIll_4var.data")


#############################################
### SAI for standardized climate values 
#############################################

for(i in c(20,5000)){
  ### i km neighbourhood window
  sai.i <- calc_SAI(scores, 
                    scores, 
                    climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i,
                    standardize = T
  )
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_",i,"kmWindow_4var_standardized.data", sep=""))
}

#############################################
###### SAIcl
#############################################

### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
colnames(scores.lgm) <- gsub("bi","bioclim", colnames(scores.lgm))
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15") 

### SAIcl within i km neighbourhood
for(i in c(20,50,100)){
  
  sai.i <- calc_SAI(data1 = scores,
                    data2 = scores.lgm,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i
                    )
  # Save
  save(sai.i, file = paste("SAIcl_", i,"kmNeighbourhood.data", sep=""))
  
}

### SAI within Whole NZ
sai.i <- calc_SAI(scores,
                  scores.lgm,
                  climateNames,
                  coordinateNames = c("x","y")
                  )
save(sai.i, file = "SAIcl_NZ_4var22sep2019.data")

