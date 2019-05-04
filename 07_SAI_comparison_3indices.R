### Comparison of indices

# Correlation matrix

library("PerformanceAnalytics")

# Load spatial similarity indices
sai <- load("SAI_5km_currentInCurrent_5000kmWindow_4var.data")
sai <- get(sai)
load("Scores_Acaena_landcover5km.data")
scores.sai <- cbind(scores, unlist(sai))
colnames(scores.sai)[length(scores.sai)] <- "SAI"

mess <- read.csv("MESS5km.csv")
colnames(mess)[length(mess)] <- "MESS"
maha <- read.csv("Mahalanobis5km.csv")
colnames(maha)[length(maha)] <-"Mahalanobis"
euc <- read.csv("Euclidean_similarity5km.csv")
colnames(euc)[length(euc)] <- "Euclidean"

# Merge all indices
dat <- merge(mess, maha, by=c("x","y"))
dat2 <- merge(dat, scores.sai, by=c("x","y"))
dat3 <- merge(dat2, euc, by=c("x","y"))

# Plot
png("SAI_comparison.png")
chart.Correlation(dat3[,c("SAI","Euclidean","Mahalanobis", "MESS")], 
                  histogram=TRUE, pch=19)
dev.off()

###########################################################################
### Where are cells with low SAI but high Euclidean similarity ?
###########################################################################

plot(dat3$Euclidean, dat3$SAI)
sai.range <- dat3$SAI < 0.55
euc.range <- dat3$Euclidean > -2000

library(rgdal)

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)

png("SAI_Euclidean_map.png")
plot(nzland)
points(dat3[sai.range & euc.range, c("x","y")],
       col="red"
       )
dev.off()
