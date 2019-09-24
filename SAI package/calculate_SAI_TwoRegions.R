
source(".\\SAI\\SAI package\\F_SpatialAvailabilityIndex.R")

library(rgdal)


##########################################################################################
#############                  SAI for Swiss data       ##################################
##########################################################################################
swiss <- read.table("Y://swiss_all_climate_2.txt", header = T)
swiss <- data.frame(swiss)

# Function
LongLatToUTM <- function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone," ellps=WGS84", sep='')))
  return(as.data.frame(res))
}

res <- LongLatToUTM(swiss$LONG, swiss$LAT, 33)
swiss[c("LATm","LONGm")] <- res[, c("Y","X")]

### Extract example areas
# Region 1
p = swiss[(swiss$LONG <= 7.65) & (swiss$LONG >= 7.4),]
reg1 <- p[(p$LAT <= 47.3) & (p$LAT >= 47),]

climateNames = colnames(reg1)[5:8]

sai.swiss <- calc_SAI(data1 = reg1,
                      data2 = reg1,
                      climateNames = climateNames,
                      coordinateNames = c("LATm","LONGm"),
                      neighbourhood.size = 25
)
# Save
save(sai.swiss, file = "SAIcc_swissRegion1_25km_neighbourhood.data")

mean(sai.swiss)



# Region 2
p = swiss[(swiss$LONG <= 7.4) & (swiss$LONG >= 7.15),]
reg2 <- p[(p$LAT <= 46.55) & (p$LAT >= 46.2),]

climateNames = colnames(reg2)[5:8]

sai.swiss2 <- calc_SAI(data1 = reg2,
                       data2 = reg2,
                       climateNames = climateNames,
                       coordinateNames = c("LATm","LONGm"),
                       neighbourhood.size = 25
)
# Save
save(sai.swiss2, file = "SAIcc_swissRegion2_25km_neighbourhood.data")

# Region 3
p = swiss[(swiss$LONG <= 8.9) & (swiss$LONG >= 8.65),]
reg3 <- p[(p$LAT <= 46.75) & (p$LAT >= 46.45),]


##########################################################################################
### SAIcc of region1 within region2
##########################################################################################

# For two different regions, SAI within a neighbourhood doesn't make sense.

sai.swiss1in2 <- calc_SAI(data1 = reg1,
                      data2 = reg2,
                      climateNames = climateNames,
                      coordinateNames = c("LATm","LONGm")
)
# Save
save(sai.swiss1in2, file = "SAIcc_of_swissRegion1_in_region2.data")

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
save(sai.swiss1in3, file = "SAIcc_of_swissRegion2_in_region3.data")


##########################################################################################
### Map
##########################################################################################


load("SAIcc_of_swissRegion2_in_region1.data")

# Combine SAI to coordinate data
coordinateNames = c("LONG","LAT")
sai.dat <- cbind(reg2[, coordinateNames], unlist(sai.swiss2in1))
colnames(sai.dat)[3] <- "SAIcc"

png("Y://sai.swissRegion2in1.png", width = 900, height = 630)
ggplot(sai.dat) + 
  geom_raster(aes_string(coordinateNames[1], coordinateNames[2], fill="SAIcc")) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

dev.off()

load("SAIcc_of_swissRegion2_in_region3.data")

# Combine SAI to coordinate data
coordinateNames = c("LONG","LAT")
sai.dat <- cbind(reg2[, coordinateNames], unlist(sai.swiss2in3))
colnames(sai.dat)[3] <- "SAIcc"

png("Y://sai.swissRegion2in3.png", width = 900, height = 630)
ggplot(sai.dat) + 
  geom_raster(aes_string(coordinateNames[1], coordinateNames[2], fill="SAIcc")) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

dev.off()

mean(unlist(sai.swiss2in1))
mean(unlist(sai.swiss2in3))

mean(reg1$prec49)
mean(reg2$prec49)
mean(reg3$prec49)
