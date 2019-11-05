###################################################################################################
### Calculate Environmental Prevalence Index (EP)
###################################################################################################

source(".\\GitHub\\Environmental-prevalence\\faster_functions_EnvironmentalPrevalenceIndex.R")
source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")

library(schoolmath)
library(doFuture)
library(foreach)
library(plyr)

### Setup for multi-core use
registerDoFuture()  ## tells foreach futures should be used
plan(multisession)  ## specifies what type of futures

### Load climate data
load("Y:\\Scores_chion_landcover_worldclim1_1km.data")

climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")


##########################################################################
###### EP of current climates under current conditions; EPcc
###########################################################################
timestamp()
### EP within Whole NZ
ep.i <- multicore_calc_EPcc_within_neighbourhood_areas(data1 = scores[1:10000,],
                                                       neighbourhood.size = 500000,
                                                      climateNames = climateNames,
                                                      coordinateNames = c("x","y")
)
timestamp()

save(ep.i, file = "EPcc_NZ_1km_4var.data")
