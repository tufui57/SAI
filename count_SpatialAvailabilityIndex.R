######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

# Data import
genus_name = "Acaena"

# LGM climate
load(paste(".\\LGM_mainisland_worldclim1_", reso, "km_scores.data", sep = ""))
# Current climate
load(paste(".\\Scores_", genus_name, "_landcover_worldclim1_", reso, "km.data", sep = ""))

dat1 <- scores
dat2 <- scores

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex1010.R")

######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################


neighbours <- count_ratioWithinNeighbourhood(dat1 = scores, dat2 = scores, 
                               coordinateNames = c("PC1", "PC2")
                               )
# save(neighbours, file = "cells_within_neighbourhood_between_current_on_PCA.data")

######################################################################################
### Calculate Spatial availability index; AUC of ratios of cells within neighbourhood
######################################################################################

scores <- calculate_auc_for_each_cell(neighbours, scores)

save(scores, file = "spatialAvailabilityIndex_between_current_on_PCA.data")

