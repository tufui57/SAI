######################################################################################
### Draw Fig. 2
######################################################################################

######################################################################################
### Data import
######################################################################################

# Current climate
load(".\\Scores_Acaena_landcover5km.data")

# Load functions
source(".//SAI//F_SpatialAvailabilityIndex.R")

# Get rid of unnneccesary columns
scores <- scores[, -grep("^Acaena", colnames(scores))]

### Choose a cell to draw Fig. 2 for by elevation
# Max elevation
maxEl <- max(scores$value, na.rm = T)

p <- scores[which(scores$value == maxEl),]

######################################################################################
### Fig. 2 (B)
######################################################################################

# Get radius sizes
a <- get_radius_size(scores)

# Count number of points within neighbourhood
neighbours <- count_ratioWithinNeighbourhood(dat1 = p,  dat2 = scores,
                                             a = a,
                                             coordinateNames = c("PC1", "PC2")
)
# Check if the above function has worked
sum(neighbours[[2]]$dat1cellnumber > 0)

plot(neighbours[[2]][, c("PC1", "PC2")])
points(neighbours[[2]][neighbours[[2]]$dat1cellnumber > 0, c("PC1", "PC2")], col="pink")
points(p[, c("PC1", "PC2")], col="red")


# Calculate SAI
AUCscores <- calculate_auc_for_each_cell(neighbours, a[[1]], scores)



