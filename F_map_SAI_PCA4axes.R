
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

# Function
plot_SAI <- function(scores, sai, time, colfunc){
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  
  sai.dat$brks <- cut(sai.dat[,time], 
                     breaks = seq(0, 1, 0.1), 
                     labels=c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3","0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                              "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))
  
  
  
  
  myplot <- ggplot(sai.dat, aes_string("x", "y", fill = "brks")) + 
    geom_raster() +
    scale_fill_manual(time, values=setNames(colfunc(11), levels(sai.dat$brks)), na.value = "transparent"
                      ) +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=20)
    )
  return(myplot)

}

hist_SAI <- function(scores, sai, time){
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- time
  
  myhist <- ggplot(sai.dat, aes_string(x = time)) +
  geom_histogram(data = sai.dat, bins = 100) +
  xlim(0,1) +
  xlab(time) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 270, vjust = 0.25),
        axis.title.y = element_text(angle = 270),
        axis.ticks.y = element_blank()
  ) +
  theme(panel.background = element_rect(fill = 'gray96'))
  
  return(myhist)

}


#### Set colour gradient
colfunc <- colorRampPalette(c("brown", "yellow", "green", "cyan", "blue", "violet", "red"))


#################################################################################
### Draw a map of SAI of current climate at the current
#################################################################################

### Load 5km data
load(".\\Scores_5km_PCA_4axes_from19.data")

# Load SAI values
load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes_from19.data")
sai <- load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes_from19.data")
sai <- get(sai)

time="SAIcc"

# Map SAI
png(paste("Y:\\", time ,"_5km_PCA4axes_from19.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai, time, colfunc)
myhist <- hist_SAI(scores, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()


###########################################################################################################
### linear regression between SAI based on original climate variables and SAI based on PC axes
###########################################################################################################
# Load SAI values
load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai <- load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai <- get(sai)

# Data of current NZ on 5km resolution
load(".\\Scores_Acaena_landcover5km.data")

### Merge the two SAIs
# Combine SAI to coordinate data
sai.or <- cbind(scores[, c("x", "y")], unlist(sai))
colnames(sai.or)[3] <- "SAIcc"

# Load SAI besed on 4 PC axes using 4 bioclim
load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes.data")
sai4 <- load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes.data")
sai4 <- get(sai4)

# Load SAI besed on 4 PC axes using 19 bioclim
load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes_from19.data")
sai19 <- load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes_from19.data")
sai19 <- get(sai19)

# data frame of occurrence data and climate data
d <- read.csv("Y://5th chapter SAI chapter//raw data//Acaena_bioclim_landcover_history_worldclim1_5km.csv")

sai.cc <- cbind(d[, c("x", "y")], unlist(sai4), unlist(sai19))
colnames(sai.cc)[3:4] <- c("SAI4", "SAI19")

# Merge the two SAIs
sais <- merge(sai.cc, sai.or, by = c("x","y"))

png("SAI_PCaxes_vs_original.png", width = 1000, height = 500)
par(mfrow = c(1,2), cex=1.3)
# Scatter plots
plot(sais$SAIcc, sais$SAI19,
     xlab = "SAI",
     ylab="SAI based on four PC axes calculated from 19 climate variables")
#summary(lm(sais$SAIcc ~ sais$SAI19))

plot(sais$SAIcc, sais$SAI4,
     xlab = "SAI",
     ylab = "SAI based on four PC axes calculated from 4 climate variables")
#summary(lm(sais$SAIcc ~ sais$SAI4))
dev.off()






#################################################################################
### Combine maps 
#################################################################################

### Load 5km data
load(".\\Scores_5km_PCA_4axes_from19.data")

# Load SAI values
load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes_from19.data")
sai19 <- load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes_from19.data")
sai19 <- get(sai19)

### Load 5km data
load(".\\Scores_5km_PCA_4axes.data")

# Load SAI values
load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes.data")
sai4 <- load("SAI_5km_currentInCurrent_5000kmWindow_PCA4axes.data")
sai4 <- get(sai4)

# Map SAI
png(paste("Y:\\", time ,"_5km_PCA4axes.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai4, time, colfunc)
myplot2 <- plot_SAI(scores, sai19, time, colfunc)

# Plot in multiple panels
grid.arrange(myplot, myplot2,
             ncol = 2, nrow = 1)
dev.off()
