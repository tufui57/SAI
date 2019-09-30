###################################################################################################
### Calculate EP for global map
###################################################################################################

source(".\\SAI\\SAI package\\F_SpatialAvailabilityIndex.R")


### EPcc for global scale
sai.i <- calc_SAI(data1 = ,
                  data2 = ,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
)
save(sai.i, file = "SAIcc_global.data")
