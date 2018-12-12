
# data frame of occurrence data and climate data
datapath <- paste("Y://bioclim_landcover_history_worldclim",
                  Worldclim, "_", reso, "km.csv", sep=""
)
dat1 <- read.csv(datapath)
d <- dat1[is.na(dat1$landCoverChange) == F, ]

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(10, 11, 18, 19), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], 
                           "x", "y", "preLandcover", "currentLandcover", "landCoverChange","value")], pca$x[, 1:2])
scores$landCoverChange <- factor(scores$landCoverChange)
scores$pre <- factor(ifelse(scores$preLandcover == 1, "NF", "nonF"))
scores$post <- factor(ifelse(scores$currentLandcover == 1, "NF",
                             ifelse(scores$currentLandcover == 3, "nonF", "non potential habitat" # EF(2) is also non potential habitat
                             ))
)

### Import PCA scores
save(scores, file = paste(".\\Scores_landcover1km.data", sep = ""))
