
##################################################################################################
### Calculate SAI of current climate in the current climate at 5km resolution
###################################################################################################

calc_SAI_p <- function(
  data1, # data.frame containing data of grid cells to calcualte SAI for.
  data2, # data.frame containing data of grid cells to search analogous climates from.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size = NULL # Size of neighbourhood area to search analogous climates (km)
  # Unit of NZTM is meter. This function might need an option for data whose coordinate unit is degree.
){
  
  ###################################################################################### 
  ### Calculate SAI
  ###################################################################################### 
  sai <- list()
  
  for(i in 1:nrow(data1)){
    
    # The target grid cell
    p <- data1[i, ]
    
    ######################################################################################
    # Prepare the neighbourhood square of the target cell
    ######################################################################################

    #  Cells to be searched for analogous climates
        neighbour.window <- data2
        
        # To get a set of climate range breadths, combine cells of data1 and data2 within the neighbourhood
        neighbour.window.data2_p <- rbind(neighbour.window[c(coordinateNames, climateNames)], 
                                          p[c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_radius_size, dat = neighbour.window.data2_p)
        names(ranges) <- climateNames
        
    ###################################################################################### 
    ### SAI calculation
    ###################################################################################### 
    sai[i] <- SAI(p, # a target grid cell
                  neighbour.window, # grid cells within the neighbourhood
                  ranges, # result of get_radius_size() or ranges_without_outliers()
                  climateNames # column names of climate variables
    )
    
    print(paste("finished the cell number", i))
  }
  
  return(sai)
}


#############################################
###### SAIcl within NZ
#############################################

### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
colnames(scores.lgm) <- gsub("bi","bioclim", colnames(scores.lgm))
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15") 


### SAI within Whole NZ
sai.i <- calc_SAI_p(data1 = scores,
                  data2 = scores.lgm,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
)
save(sai.i, file = "SAIcl_NZ_p.data")

#############################################
###### SAIcl for Swiss
#############################################


sai.swiss2in1 <- calc_SAI_p(data1 = reg2,
                          data2 = reg1,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss2in1, file = "SAIcc_of_swissRegion2_in_region1_rangeBreadthP_reg1.data")

sai.swiss2in3 <- calc_SAI_p(data1 = reg2,
                          data2 = reg3,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss2in3, file = "SAIcc_of_swissRegion2_in_region3_rangeBreadthP_reg3.data")

##########################################################################################
### Map
##########################################################################################
library(ggplot2)
load("SAIcc_of_swissRegion2_in_region1_rangeBreadthP_reg1.data")

# Combine SAI to coordinate data
coordinateNames = c("LONG","LAT")
sai.dat <- cbind(reg2[, coordinateNames], unlist(sai.swiss2in1))
colnames(sai.dat)[3] <- "SAIcc"

png("Y://sai.swissRegion2in1_p.png", width = 900, height = 630)
ggplot(sai.dat) + 
  geom_raster(aes_string(coordinateNames[1], coordinateNames[2], fill="SAIcc")) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

dev.off()

load("SAIcc_of_swissRegion2_in_region3_rangeBreadthP_reg3.data")

# Combine SAI to coordinate data
coordinateNames = c("LONG","LAT")
sai.dat <- cbind(reg2[, coordinateNames], unlist(sai.swiss2in3))
colnames(sai.dat)[3] <- "SAIcc"

png("Y://sai.swissRegion2in3_p.png", width = 900, height = 630)
ggplot(sai.dat) + 
  geom_raster(aes_string(coordinateNames[1], coordinateNames[2], fill="SAIcc")) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

dev.off()
