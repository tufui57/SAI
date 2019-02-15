###################################################################################################
### Do variables have to be standardized for SAI calculation?
###################################################################################################

# See the difference between SAI using standardized variables and normal ones
scores.sc <- cbind(scores[, !(colnames(scores) %in% coordinateNames)], scale(scores[, coordinateNames]))

load(".\\Scores_Acaena_landcover5km.data")


for(i in coordinateNames){
  png(paste(i,"_hist.png", sep=""))
  par(mfrow=c(1,2))
  hist(scores.sc[,i], xlab=i)
  hist(scores[,i], xlab=i)
  dev.off()
}


### How different standardized SAI and non standardized SAI.
load("SAI_5km_currentInCurrent_20kmWindow_4var_standardized.data")
nam <- load("SAI_5km_currentInCurrent_20kmWindow_4var_standardized.data")
sai.st <- get(nam)

load("SAI_5km_currentInCurrent_1500kmWindow_4var.data")

sum(unlist(sai.i) == unlist(sai.st))

### They are 99.9% same!

### Double check

source(".\\SAI\\F_DEBUG_SpatialAvailabilityIndex.R")

SAI_for_area <- function(time, # "current" or "LGM"
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
  
  # Set ranges of each variable
  ranges <- lapply(coordinateNames, get_radius_size, dat = scores)
  names(ranges) <- coordinateNames
  
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

    
    ### SAI calculation
    sai[i] <- SAI(p, # a point at the centre of search area
                  neighbour.window, # data of points to be searched
                  ranges, # result of get_radius_size()
                  coordinateNames # column name for climate variable
    )
  }
  
  return(sai)
}
### 20 km neighbourhood window
sai20 <- SAI_for_area("current", 20, standardize = FALSE, whole=F)
sai20.st <- SAI_for_area("current", 20, standardize = TRUE, whole=F)

plot(unlist(sai20), unlist(sai20.st))






### Check location of centre point
library(rgdal)
map <- readOGR("Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp")
plot(map)
points(p[,c("x","y")],col="red")

### The following must be within range[coordinate1][11]
lapply(neighbours.size[[11]][, coordinateNames], summary)


#####################################################################################
## Test data
######################################################################################

source(".\\SAI\\F_modified_SpatialAvailabilityIndex.R")

load(".\\Scores_Acaena_landcover5km.data")
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")
# Set ranges of each variable
ranges <- lapply(coordinateNames, get_radius_size, dat = scores)
names(ranges) <- coordinateNames

## Test window setting function
neighbour.window.size = 200

# the number of cells in neighbour.window < or = ((window size/ 2 / cell resolution) * 2 + 1)^2 - 1
hist(sapply(neighbour.window, nrow))

