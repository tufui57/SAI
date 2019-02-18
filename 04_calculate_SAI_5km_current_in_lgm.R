######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")

######################################################################################
### Data import
######################################################################################

# the LGM climate scores
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
# Current climate scores
load(".\\Scores_Acaena_landcover5km.data")

######################################################################################
### Calculate Spatial availability index of current climnate at the present
######################################################################################

# name of variables in searched data
coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")
# Colnames for target variables of current and LGM scores must be shared.
colnames(scores.lgm) <- gsub("bi", "bioclim", colnames(scores.lgm))

# Range of variables in searched data
ranges <- lapply(coordinateNames, get_radius_size, dat = scores.lgm)
names(ranges) <- coordinateNames



SAIcl_for_area <- function(neighbour.window.size, # Size of windows to calculate the SAI (km)
                         whole
){
  
  sai <- list()
  for(i in 1:nrow(scores)){
    
    #  p must have variable values of the current
    p <- scores[i, ]
    
    
    # neighbour.window must have variable values of the LGM
    if(whole == F){
      ######################################################################################
      # Prepare a x a km2 neighbourhood window as an area to be searched
      ######################################################################################
      # For the first step, try a = 20
      # Unit of NZTM is meter, so 10000 m = 10km = 20km/2
      a <- neighbour.window.size * 1000 / 2
      dat.x <- Count_cells_within_neighbourhood(p, scores.lgm, a, "x")
      neighbour.window <- Count_cells_within_neighbourhood(p, dat.x, a, "y")
      
    }else{
      # Whole NZ is the neghbourhood
      neighbour.window <- scores.lgm
      
    }
    
    ### SAI calculation
    sai[i] <- SAI(p, # a point at the centre of search area.
                  neighbour.window, # data of points to be searched. 
                  ranges, # result of get_radius_size()
                  coordinateNames # column names for climate variables in p and neighbour.window
    )
  }
  
  return(sai)

}



###### SAI of current in the LGM

for(i in c(20)){
  ### i km neighbourhood window
  sai.i<- SAIcl_for_area(i, whole=F)
  # Save
  save(sai.i, file = paste("SAI_5km_currentInLGM_", i,"kmWindow_4var_test.data", sep=""))
  
}

# Whole NZ
sai.i <- SAIcl_for_area(5000,  whole=T)
# Save
save(sai.i, file = "SAI_5km_currentInLGM_5000kmWindow_4var.data")
