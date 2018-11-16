######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

######################################################################################
### Data import
######################################################################################

# LGM climate
load(paste(".\\LGM_mainisland_worldclim1_5km_scores.data", sep = ""))
# Current climate
load(paste(".\\Scores_Acaena_landcover_worldclim1_5km.data", sep = ""))

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")

# Get rid of unnneccesary columns
scores <- scores[, -grep("^Acaena", colnames(scores))]

######################################################################################
### Calculate Spatial availability index of current climnate at the present
######################################################################################

# Get radius sizes
a <- get_radius_size(scores)

# Count number of points within neighbourhood
neighbours <- count_ratioWithinNeighbourhood(dat1 = scores, dat2 = scores, 
                                             a = a,
                               coordinateNames = c("PC1", "PC2")
                               )
# Calculate SAI
AUCscores <- calculate_auc_for_each_cell(neighbours, a[[1]], scores)

save(AUCscores, file = "spatialAvailabilityIndex_5km_of_current_on_PCA.data")

######################################################################################
### Calculate Spatial availability index of LGM climnate at the LGM
######################################################################################

# # SAI for cells within current NZ area
# neighbours <- count_ratioWithinNeighbourhood(dat1 = newdf, dat2 = newdf, 
#                                              coordinateNames = c("PC1", "PC2")
# )
# 
# AUCscores <- calculate_auc_for_each_cell(neighbours, scores)
# 
# save(AUCscores, file = "spatialAvailabilityIndex_5km_of_LGM_on_PCA.data")

# SAI for cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

neighbours <- count_ratioWithinNeighbourhood(dat1 = scores.lgm, dat2 = scores.lgm, 
                                             coordinateNames = c("PC1", "PC2")
)

AUCscores <- calculate_auc_for_each_cell(neighbours, scores.lgm)

save(AUCscores, file = "spatialAvailabilityIndex_5km_of_LGM_on_PCA_withinLGMland.data")

