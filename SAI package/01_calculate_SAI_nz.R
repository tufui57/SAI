###################################################################################################
### Calculate SAI
###################################################################################################

source(".\\SAI\\SAI package\\F_SpatialAvailabilityIndex.R")


############################################
###### Current
#############################################

### Load current cliamte data at 5km resolution
load(".\\Scores_Acaena_landcover5km.data")
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

### SAI within i km neighbourhood
for(n in c(20,50,100
           )){
  
  sai.i <- calc_SAI(data1 = scores,
                    data2 = scores,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = n
                    )
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_", n,"kmNeighbourhood.data", sep=""))
  
}

### SAI within Whole NZ
sai.i <- calc_SAI(data1 = scores,
                  data2 = scores,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
                  )
save(sai.i, file = "SAIcc_NZ_4var.data")

###### Outlier removal

for(i in c(1, 2.5, 5)){
  # Whole NZ
  sai.i <- calc_SAI(data1 = scores,
                    data2 = scores,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    outlierRemoval = T,
                    outlierPercent = i
  )
  # Save
  save(sai.i, file = paste("SAIcc_NZ_outlier", i, ".data", sep=""))
  
}



#############################################
###### LGM
#############################################

# data of LGM mainland
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
climateNames <- c("bi1", "bi6", "bi12", "bi15")

for(i in c(20,50,100)){
  ### i km neighbourhood window
  sai.i <- calc_SAI(scores.lgm,
                    scores.lgm,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i
  )
  # Save
  save(sai.i, file = paste("SAI_5km_LGMInLGM_", i,"kmNeighbourhood.data", sep=""))
  
}

# Whole NZ
sai.i <- calc_SAI(scores.lgm, 
                  scores.lgm, 
                  climateNames,
                  coordinateNames = c("x","y")
                  )
# Save
save(sai.i, file = "SAIll_4var.data")


#############################################
### SAI for standardized climate values 
#############################################

for(i in c(20,5000)){
  ### i km neighbourhood window
  sai.i <- calc_SAI(scores, 
                    scores, 
                    climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i,
                    standardize = T
  )
  # Save
  save(sai.i, file = paste("SAI_5km_currentInCurrent_",i,"kmWindow_4var_standardized.data", sep=""))
}

#############################################
###### SAIcl
#############################################

### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
colnames(scores.lgm) <- gsub("bi","bioclim", colnames(scores.lgm))
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15") 

### SAIcl within i km neighbourhood
for(i in c(20,50,100)){
  
  sai.i <- calc_SAI(data1 = scores,
                    data2 = scores.lgm,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i
                    )
  # Save
  save(sai.i, file = paste("SAIcl_", i,"kmNeighbourhood.data", sep=""))
  
}

### SAI within Whole NZ
sai.i <- calc_SAI(scores,
                  scores.lgm,
                  climateNames,
                  coordinateNames = c("x","y")
                  )
save(sai.i, file = "SAIcl_NZ_4var22sep2019.data")

