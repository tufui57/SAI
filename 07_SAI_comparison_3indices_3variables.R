### Comparison of indices

# Load spatial similarity indices
sai <- load("SAI_5km_currentInCurrent_5000kmWindow_3var.data")
sai <- get(sai)
load("Y://5th chapter SAI chapter//raw data//Scores_Acaena_landcover5km.data")
scores.sai <- cbind(scores, unlist(sai))
colnames(scores.sai)[length(scores.sai)] <- "SAI"

mess <- read.csv("MESS5km_3variables.csv")
colnames(mess)[length(mess)] <- "MESS"
maha <- read.csv("Mahalanobis5km_3variables.csv")
colnames(maha)[length(maha)] <-"Mahalanobis"
euc <- read.csv("Euclidean_similarity5km_3variables.csv")
colnames(euc)[length(euc)] <- "Euclidean"

# Merge all indices
dat <- merge(mess, maha, by=c("x","y"))
dat2 <- merge(dat, scores.sai, by=c("x","y"))
dat3 <- merge(dat2, euc, by=c("x","y"))

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

png("Y://SAI_comparison_3variables.png")
pairs(dat3[,c("SAI","Euclidean","MESS", "Mahalanobis")],
      diag.panel = panel.hist(x, ylim = c(0, 5000)), upper.panel = panel.cor,lower.panel = panel.sca
)
dev.off()



