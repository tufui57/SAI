######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

# Data import
genus_name = "Acaena"

load(paste(".\\Scores_", genus_name,"_landcover_worldclim1_1km.data", sep = ""))

dat1 <- scores[1:2,]
dat2 <- scores

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")

######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################

Count_cells_within_neighbourhood <- function(dat1, # data of points to be searched
                                             dat2, # data of points which are centre of search area
                                             a1, a2, # half length (unit of the distance is the unit of coordinate) of the squire which you want to count the number of avairable secondary open cells.
                                             coordinateNames # column names for coordinates in dat1 and dat2
){
  # Add cell ID to dat2
  dat2$cellID <- 1:nrow(dat2)
  
  # Try decrease data size of "dat2"
  dat2 <- dat2[, c(coordinateNames, "cellID")]
  # Count the number of secondary open cells within squire of the distance "a"
  
  dat1_in_dat2area <- lapply(dat2$cellID, function(i){
    
    # Find dat1 points within dat2 point -a <= dat1 point <= dat2 point + a 
    datlat <- dat1[(dat1[, coordinateNames[2]] <= (dat2[i, coordinateNames[2]] + a2) & dat1[, coordinateNames[2]] >= (dat2[i, coordinateNames[2]] - a2)), ]
    datlatlon <- datlat[(datlat[, coordinateNames[1]] <= (dat2[i, coordinateNames[1]] + a1) & datlat[, coordinateNames[1]] >= (dat2[i, coordinateNames[1]] - a1)), ]
    
    
    # Name the group of dat1 points with cell ID of dat2
    # If the "dat2cellID" of a dat2 row is 1, the point of the dat2 row is within the neighbourhood of row 1 of dat2
    datlatlon$dat2cellID <- rep(i, nrow(datlatlon))
    return(datlatlon)
  }
  )
  
  # Count the number of cells within neighbourhood
  dat2$dat1cellnumber <- sapply(dat1_in_dat2area, nrow)
  
  return(dat2)
}

### Count cells within neighbourhood
# This step requires memory > 64MB, so half of the radius sizes should be executed.

coordinateNames = c("PC1","PC2")

neighbours1 <- list()
  
# First cycle
for(i in 2:round(length(a1)/3)){
  neighbours1[[i]] <- Count_cells_within_neighbourhood(dat1, dat2, 
                                                      a1 = a1[i], a2 = a2[i], coordinateNames)
  
  # Calculate percentage of area within the neighbourhood over NZ
  neighbours1[[i]]$ratioWithinNeighbourhood <- neighbours[[i]]$dat1cellnumber / nrow(dat1)
  
}

scores1 <- calculate_auc_for_each_cell(neighbours1, scores)
save(scores1, file = "SAI_1km_of_current_01.data")
# Second cycle
neighbours2 <- list()
for(i in (round(length(a1)/3)+1):(round(length(a1)/3)*2)){
  neighbours2[[i]] <- Count_cells_within_neighbourhood(dat1, dat2, 
                                                      a1 = a1[i], a2 = a2[i], coordinateNames)
  
  # Calculate percentage of area within the neighbourhood over NZ
  neighbours2[[i]]$ratioWithinNeighbourhood <- neighbours[[i]]$dat1cellnumber / nrow(dat1)
  
}

scores2 <- calculate_auc_for_each_cell(neighbours2, scores)
save(scores2, file = "SAI_1km_of_current_02.data")

# Third cycle
neighbours3 <- list()
for(i in (round(length(a1)/3)*2 + 1):length(a1)){
  neighbours3[[i]] <- Count_cells_within_neighbourhood(dat1, dat2, 
                                                      a1 = a1[i], a2 = a2[i], coordinateNames)
  
  # Calculate percentage of area within the neighbourhood over NZ
  neighbours3[[i]]$ratioWithinNeighbourhood <- neighbours[[i]]$dat1cellnumber / nrow(dat1)
  
}

scores3 <- calculate_auc_for_each_cell(neighbours3, scores)
save(scores3, file = "SAI_1km_of_current_03.data")

