######################################################################################
### Draw Fig. 2
######################################################################################

######################################################################################
### Data import
######################################################################################

# Current climate
# 5km
load(".\\Scores_Acaena_landcover5km.data")
# 1km
load(".\\Scores_Acaena_landcover18sep.data")

# Load functions
source(".//SAI//F_modified_SpatialAvailabilityIndex.R")

# Get rid of unnneccesary columns
scores <- scores[, -grep("^Acaena", colnames(scores))]

### Choose a cell to draw Fig. 2 for by elevation

p <- scores[10000,]

######################################################################################
### Fig. 2 (B)
######################################################################################

# Get radius sizes
a <- get_radius_size(scores)

# Count number of points within neighbourhood for axis1
neighbours <- count_ratioWithinNeighbourhood(p, # data of points to be searched
                                             scores, # data of points which are centre of search area
                                             a = a[[1]],
                                             coordinateName)

# Check if the above function has worked
plot(scores[, c("PC1", "PC2")])
points(neighbours[[2]][[1]][, c("PC1", "PC2")], col="pink")
points(p[, c("PC1", "PC2")], col="red")


# Calculate SAI
AUCscores <- calculate_auc_for_each_cell(neighbours, a[[1]], scores)



