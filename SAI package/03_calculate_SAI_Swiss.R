source(".\\EP\\functions_EnvironmentalPrevalenceIndex.R")

library(rgdal)


##########################################################################################
#############                  SAI for Swiss data       ##################################
##########################################################################################
# swiss <- read.table("Y://swiss_all_climate_2.txt", header = T)
# swiss <- data.frame(swiss)
# 
# # Function
# LongLatToUTM <- function(x,y,zone){
#   xy <- data.frame(ID = 1:length(x), X = x, Y = y)
#   coordinates(xy) <- c("X", "Y")
#   proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
#   res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone," ellps=WGS84", sep='')))
#   return(as.data.frame(res))
# }
# 
# res <- LongLatToUTM(swiss$LONG, swiss$LAT, 33)
# swiss[c("LATm","LONGm")] <- res[, c("Y","X")]
# 
# write.csv(swiss, file = "Y://swiss_all_climate_2.csv")

swiss <- read.csv("Y://swiss_all_climate_2.csv")
### Extract example areas

# For regions with no natural borders, additional cells in the neighbourhood of cells at the edge of target region is needed.
# Region 1 incl. reference areas
p = swiss[(swiss$LONG <= (7.65 + 0.25)) & (swiss$LONG >= (7.4 - 0.25)), ]
reg1_r <- p[(p$LAT <= (47.3 + 0.4)) & (p$LAT >= (47 - 0.3)), ]

# Region 1
p = swiss[(swiss$LONG <= 7.65) & (swiss$LONG >= 7.4),]
reg1 <- p[(p$LAT <= 47.3) & (p$LAT >= 47),]

climateNames = colnames(reg1)[5:8]

sai.swiss <- calc_EP(data1 = reg1,
                      data2 = reg1_r,
                      climateNames = climateNames,
                      coordinateNames = c("LATm","LONGm"),
                      neighbourhood.size = 50
)
# Save
save(sai.swiss, file = "SAIcc_swissRegion1_50km_neighbourhood.data")

mean(sai.swiss)



# Region 2
p = swiss[(swiss$LONG <= (7.4 + 0.25)) & (swiss$LONG >= (7.15-0.25)), ]
reg2_r <- p[(p$LAT <= (46.525 + 0.3)) & (p$LAT >= (46.225-0.3)),]

# Region 2
p = swiss[(swiss$LONG <= 7.4) & (swiss$LONG >= 7.15),]
reg2 <- p[(p$LAT <= 46.525) & (p$LAT >= 46.225),]

climateNames = colnames(reg2)[5:8]

sai.swiss2 <- calc_EP(data1 = reg2,
                       data2 = reg2_r,
                       climateNames = climateNames,
                       coordinateNames = c("LATm","LONGm"),
                       neighbourhood.size = 50
)
# Save
save(sai.swiss2, file = "SAIcc_swissRegion2_50km_neighbourhood.data")

# Region 3
p = swiss[(swiss$LONG <= (8.9+0.25)) & (swiss$LONG >= (8.65-0.25)), ]
reg3_r <- p[(p$LAT <= (46.75+0.3)) & (p$LAT >= (46.45-0.3)),]
# Region 3
p = swiss[(swiss$LONG <= 8.9) & (swiss$LONG >= 8.65),]
reg3 <- p[(p$LAT <= 46.75) & (p$LAT >= 46.45),]

sai.swiss3 <- calc_EP(data1 = reg3,
                       data2 = reg3_r,
                       climateNames = climateNames,
                       coordinateNames = c("LATm","LONGm"),
                       neighbourhood.size = 50
)
# Save
save(sai.swiss3, file = "SAIcc_swissRegion3_50km_neighbourhood.data")


##########################################################################################
### SAIcc of region1 within region2
##########################################################################################

# Region 1
p = swiss[(swiss$LONG <= 7.65) & (swiss$LONG >= 7.4),]
reg1 <- p[(p$LAT <= 47.3) & (p$LAT >= 47),]

# Region 2
p = swiss[(swiss$LONG <= 7.4) & (swiss$LONG >= 7.15),]
reg2 <- p[(p$LAT <= 46.525) & (p$LAT >= 46.225),]

# Region 3
p = swiss[(swiss$LONG <= 8.9) & (swiss$LONG >= 8.65),]
reg3 <- p[(p$LAT <= 46.75) & (p$LAT >= 46.45),]


# For two different regions, SAI within a neighbourhood doesn't make sense.

sai.swiss1in2 <- calc_SAI(data1 = reg1,
                          data2 = reg2,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss1in2, file = "SAIcc_of_swissRegion1_in_region2.data")

# tes1 <- sai.swiss1in2
# mean(unlist(tes1))

sai.swiss1in3 <- calc_SAI(data1 = reg1,
                          data2 = reg3,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss1in3, file = "SAIcc_of_swissRegion1_in_region3.data")

sai.swiss2in1 <- calc_SAI(data1 = reg2,
                          data2 = reg1,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss2in1, file = "SAIcc_of_swissRegion2_in_region1.data")

sai.swiss2in3 <- calc_SAI(data1 = reg2,
                          data2 = reg3,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss2in3, file = "SAIcc_of_swissRegion2_in_region3.data")

sai.swiss3in1 <- calc_SAI(data1 = reg3,
                          data2 = reg1,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss3in1, file = "SAIcc_of_swissRegion3_in_region1.data")

sai.swiss3in2 <- calc_SAI(data1 = reg3,
                          data2 = reg2,
                          climateNames = climateNames,
                          coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss3in2, file = "SAIcc_of_swissRegion3_in_region2.data")
