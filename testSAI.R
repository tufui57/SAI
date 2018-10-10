######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

# Data import
genus_name = "Acaena"

load(paste(".\\Scores_", genus_name,"_landcover_worldclim1_1km.data", sep = ""))

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")

# Prepare radius size vector
# Range of environmental variable * 0, 10%, ..., 90%, 100%

a1 <- ((max(scores[,"PC1"]) - min(scores[,"PC1"]) ) * (c(0, 1:10) / 10)) /2
a2 <- ((max(scores[,"PC2"]) - min(scores[,"PC2"]) ) * (c(0, 1:10) / 10)) /2

######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################

### Count cells within neighbourhood

neighbours <- list()

# The first radius size (in a1 & a2) is 0, so skip it to count cells within neighbourhood
for(i in 2:length(a1)){
  neighbours[[i]] <- Count_cells_within_neighbourhood(dat1 = scores, dat2 = scores, 
                                                      a1 = a1[i], a2 = a2[i], coordinateNames = c("PC1", "PC2"))

  # Calculate percentage of area within the neighbourhood over NZ
  neighbours[[i]]$ratioWithinNeighbourhood <- neighbours[[i]]$dat1cellnumber / nrow(newdf)
  
}

save(neighbours, file = "cells_within_neighbourhood_between_current_on_PCA.data")

######################################################################################
### Calculate Spatial availability index; AUC of ratios of cells within neighbourhood
######################################################################################

### Calculate AUC (area under curve) doe each current grid cells

for(i in 1:nrow(scores)){
  
  # Extract ratio of cells within neighbourhood at each size of radius
  cell1 <- sapply(2:length(a1), function(j){neighbours[[j]][i,"ratioWithinNeighbourhood"]
  }
  )
  # Plot the curve
  # plot(c(0, 1:10), c(0,cell1))
  
  # AUC
  scores[i,"AUC"] <- auc(c(0, 1:10)*0.1, c(0, cell1))
}

save(scores, file = "spatialAvailabilityIndex_between_current_on_PCA.data")

