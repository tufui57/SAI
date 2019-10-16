########################################
### Environmental prevalence of the world 
########################################

library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)
library(schoolmath)
library(doFuture)
library(foreach)
library(plyr)
library(dplyr)
source(".\\GitHub\\functions\\F04_convert_Points_to_raster.R")
source(".\\GitHub\\Environmental-prevalence\\faster_functions_EnvironmentalPrevalenceIndex.R")
source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")

### Setup for multi-core use
registerDoFuture()  ## tells foreach futures should be used
plan(multisession)  ## specifies what type of futures

#################################################################################
### Function to daw a map of spatial availability index 
#################################################################################

# Function
plot_SAI <- function(scores, sai, nameOfsai, 
                     coordinateNames, # the order must be c("x", "y")
                     colfunc){
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, coordinateNames], unlist(sai))
  colnames(sai.dat)[3] <- nameOfsai
  
  sai.dat$brks <- cut(sai.dat[,nameOfsai], 
                      breaks = seq(0, 1, 0.1), 
                      labels=c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3","0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6",
                               "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0")
  )
  
  myplot <- ggplot(sai.dat, 
                   aes_string(coordinateNames[1], coordinateNames[2], fill = "brks")
  ) + 
    geom_raster() +
    scale_fill_manual(nameOfsai, values=setNames(colfunc(11), levels(sai.dat$brks)), na.value = "transparent"
    ) +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=20)
    )
  return(myplot)
  
}

hist_SAI <- function(scores, sai, nameOfsai){
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- nameOfsai
  
  myhist <- ggplot(sai.dat, aes_string(x = nameOfsai)) +
    geom_histogram(data = sai.dat, bins = 100) +
    xlim(0,1) +
    xlab(nameOfsai) +
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


########################################
### Data preparation
########################################
files <- list.files("Y:\\Writings\\Thesis\\3rd chapter\\Publication\\Climate data")
files2 <- paste("Y:\\Writings\\Thesis\\3rd chapter\\Publication\\Climate data\\", files[grepl("^05", files)], sep = "")

wor <- lapply(files2, read.table, header = T)

wor1 <- cbind(sapply(wor, function(x){return(x[,4])}))

wor2 <- cbind(wor[[1]][, c("x","y")], wor1)
colnames(wor2) <- c("x","y", paste("bio", c(1,6,12,15), sep=""))


climateNames = paste("bio", c(1,6,12,15), sep="")

wor2 <- wor2[complete.cases(wor2),]

system.time(
  ep.world <- multicore_calc_EPcc_within_whole_target_areas(data1 = wor2,
                      climateNames = climateNames,
                      coordinateNames = c("x","y")
                      )
)
# Save
save(ep.world, file = "EPcc_worldtest.data")


# Map
wor.d <- cbind(wor2[, c("x","y")], unlist(ep.world)) %>%  as.data.frame
colnames(wor.d) <- c("Longitude", "Latitude", "val")

png("Y:\\worldEPmap.png", width = 1300, height = 630)


myplot <- plot_SAI(wor2, ep.world, "EPcc", c("x","y"), colfunc)
myhist <- hist_SAI(wor2, ep.world, "EPcc")

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))

dev.off()

