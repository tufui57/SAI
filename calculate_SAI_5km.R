######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

# Data import
genus_name = "Acaena"

# LGM climate
load(paste(".\\LGM_mainisland_worldclim1_5km_scores.data", sep = ""))
# Current climate
load(paste(".\\Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))

dat1 <- scores
dat2 <- scores

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")


######################################################################################
### Calculate Spatial availability index of current climnate at the present
######################################################################################

neighbours <- count_ratioWithinNeighbourhood(dat1 = scores, dat2 = scores, 
                               coordinateNames = c("PC1", "PC2")
                               )

scores <- calculate_auc_for_each_cell(neighbours, scores)

save(scores, file = "spatialAvailabilityIndex_5km_of_current_on_PCA.data")

######################################################################################
### Calculate Spatial availability index of LGM climnate at the LGM
######################################################################################

# SAI for cells within current NZ area
neighbours <- count_ratioWithinNeighbourhood(dat1 = newdf, dat2 = newdf, 
                                             coordinateNames = c("PC1", "PC2")
)

scores <- calculate_auc_for_each_cell(neighbours, scores)

save(scores, file = "spatialAvailabilityIndex_5km_of_LGM_on_PCA.data")

# SAI for cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

neighbours <- count_ratioWithinNeighbourhood(dat1 = scores.lgm, dat2 = scores.lgm, 
                                             coordinateNames = c("PC1", "PC2")
)

scores.lgm2 <- calculate_auc_for_each_cell(neighbours, scores.lgm)

save(scores.lgm2, file = "spatialAvailabilityIndex_5km_of_LGM_on_PCA_withinLGMland.data")

