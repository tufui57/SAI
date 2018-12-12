######################################################################################
### Draw Fig. 2
######################################################################################

######################################################################################
### Data import
######################################################################################

# Current climate
# 5km
# load(".\\Scores_Acaena_landcover5km.data")
# 1km
load(".\\Scores_landcover.data")

# Load functions
source(".//SAI//F_modified_SpatialAvailabilityIndex.R")

### Choose a cell to draw Fig. 2 for by elevation

# For test
p <- scores[10000,]
coordinateName = "bioclim10"

######################################################################################
### Fig. 2 (B)
######################################################################################

# Get radius sizes
a <- get_radius_size(scores, coordinateName)

# Count number of points within neighbourhood for axis1
neighbours <- count_ratioWithinNeighbourhood(p, # data of points which are centre of search area
                                             scores, # data of points to be searched
                                             a = a,
                                             coordinateName
                                             )

min(scores[, coordinateName])
max(scores[, coordinateName])
p[, coordinateName]
a
sapply(neighbours, "[[", 2)

# Check if the above function has worked
plot(scores[, coordinateName])
points(neighbours[[11]][[1]][, coordinateName], col="pink")
points(p[, coordinateName], col="red")


# Calculate SAI
AUCscores <- calculate_auc_for_each_cell(neighbours, a[[1]], scores)



