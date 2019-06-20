#######################################################
### Comparison of similarity indices with SAI
#######################################################

# Correlation matrix
setwd("Y://5th chapter SAI chapter//meta data")

# Load spatial similarity indices
sai <- load("SAI_5km_currentInCurrent_5000kmWindow_4var.data")
sai <- get(sai)
load("Y://Scores_Chionochloa_landcover5km.data")
scores.sai <- cbind(scores, unlist(sai))
colnames(scores.sai)[length(scores.sai)] <- "SAI"

mess <- read.csv("MESS5km.csv")
colnames(mess)[length(mess)] <- "MESS"
maha <- read.csv("Mahalanobis5km.csv")
colnames(maha)[length(maha)] <-"Mahalanobis"
euc <- read.csv("Euclidean_similarity5km.csv")
colnames(euc)[length(euc)] <- "Euclidean"

# Merge all indices
dat <- merge(mess[,c("x", "y", "MESS")], maha[,c("x", "y", "Mahalanobis")], by=c("x","y"))
dat2 <- merge(dat[,c("x", "y", "MESS", "Mahalanobis")], euc[,c("x", "y", "Euclidean")], by=c("x","y"))
dat3 <- merge(dat2[,c("x", "y", "MESS", "Mahalanobis","Euclidean")], scores.sai, by=c("x","y"))

# Plot
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.sca <- function(x, y, ...) {
  points(x, y, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25),
         cex=0.05)
}

png("Y://SAI_comparison_4variables.png")
pairs(dat3[,c("SAI","Euclidean","MESS", "Mahalanobis")],
      diag.panel=panel.hist, upper.panel = panel.cor,lower.panel = panel.sca
      )
dev.off()

###########################################################################
### Where are cells with low SAI but high Euclidean similarity ?
###########################################################################

plot(dat3$Euclidean, dat3$SAI)
sai.range <- dat3$SAI < 0.55
euc.range <- dat3$Euclidean > -2000

sum(is.na(dat3[sai.range & euc.range, c("bioclim1","bioclim6","bioclim12", "bioclim15")]))

lapply(c("bioclim1","bioclim6","bioclim12", "bioclim15"), 
       function(x){
         hist(dat3[, x], main = x)
         hist(dat3[sai.range & euc.range, x], col ="red", add=T)
       }
       )

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



###########################################################################
### Why the frequency histograms look wierd?
###########################################################################

png("Y://sai_maha.png")
hist(scale(dat3$SAI), col = rgb(1,0,0, alpha = 0.4),
     xlim=c(-10,10), ylim=c(0,8000), 
     main="SAI vs Mahalanobis", xlab="scaled values")
hist(scale(dat3$Mahalanobis), add=TRUE, col=rgb(0,1,0, alpha = 0.4))
dev.off()

png("Y://sai_euc.png")
hist(scale(dat3$SAI), col = rgb(1,0,0, alpha = 0.4),
     xlim=c(-10,10), ylim=c(0,3500), 
     main="SAI vs Euclidean", xlab="scaled values")
hist(scale(dat3$Euclidean), add=TRUE, col=rgb(0.5,0.5,0.5, alpha = 0.4))
dev.off()

png("Y://sai_mess.png")
hist(scale(dat3$MESS), col = rgb(0,0,1, alpha = 0.4),
     xlim = c(-10,10), ylim = c(0,3500), 
     main = "SAI vs MESS", xlab="scaled values")
hist(scale(dat3$SAI), add=TRUE, col=rgb(1,0,0, alpha = 0.4))
dev.off()
