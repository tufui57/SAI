###################################################################################################
### Calculate Environmental Prevalence Index (EP)
###################################################################################################

source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")
library(ggplot2)

##########################################################################
###### EP of current climates under current conditions; EPcc
###########################################################################

### Load current cliamte data at 5km resolution
load(".\\Scores_Acaena_landcover5km.data")
climateName <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

### EP within Whole NZ

for(i in climateName){
  ep <- calc_EP(data1 = scores,
                  data2 = scores,
                  climateNames = i,
                  coordinateNames = c("x","y")
                  )
  ### Check the map of EP
  ggplot(ep) +
    geom_raster(aes_string(x = "x", y = "y", fill="EP")) +
    ggtitle(i)

  save(ep, file = paste("EPcc_NZ_", i,".data", sep = ""))
}






