
# data frame of occurrence data and climate data
datapath <- "Y://Acaena_bioclim_landcover_history_worldclim1_5km.csv"
dat1 <- read.csv(datapath)
d <- dat1[is.na(dat1$landCoverChange) == F, ]

# species names
sname <- colnames(d)[grepl(paste("^", genus_name, sep = ""), colnames(d))]

for(i in sname){
  d[is.na(d[,i]),i] <- 0
}



# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], sname,
                           "x", "y", "preLandcover", "currentLandcover", "landCoverChange","value")], pca$x[, 1:2])
scores$landCoverChange <- factor(scores$landCoverChange)
scores$pre <- factor(ifelse(scores$preLandcover == 1, "NF", "nonF"))
scores$post <- factor(ifelse(scores$currentLandcover == 1, "NF",
                             ifelse(scores$currentLandcover == 3, "nonF", "non potential habitat" # EF(2) is also non potential habitat
                             ))
)

### Import PCA scores
save(scores, file = paste(".\\Scores_", genus_name, "_landcover5km.data", sep = ""))
