
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
  
  myplot <- ggplot(sai.dat, aes_string("x", "y", fill = time)) + 
    geom_raster() +
    scale_fill_gradientn(colours = colfunc(30), na.value = "transparent",
                         breaks=c(0, 0.5, 1),
                         limits=c(0,1)
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
  geom_histogram(data = sai.dat) +
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
### Draw a map of SAI of LGM climate at the LGM
#################################################################################

### Load data of cells within the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

# # Load SAI values
load("SAI_5km_LGMInLGM_5000kmWindow_4var.data")
nam <- load("SAI_5km_LGMInLGM_5000kmWindow_4var.data")
sai <- get(nam)

time="SAIll"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores.lgm, sai, time, colfunc)
myhist <- hist_SAI(scores.lgm, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()


#################################################################################
### Draw a map of SAI of current climate at the current
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai <- load("SAI_5km_currentIncurrent_5000kmWindow_4var.data")
sai <- get(sai)

time="SAIcc"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai, time, colfunc)
myhist <- hist_SAI(scores, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()

#################################################################################
### Draw a map of SAI of current climate at the LGM
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("SAI_5km_currentInLGM_5000kmWindow_4var.data")
sai <- load("SAI_5km_currentInLGM_5000kmWindow_4var.data")
sai <- get(sai)

time="SAIcl"

# Map SAI
png(paste("Y:\\", time ,"_5km.png", sep = ""), width = 900, height = 630)

myplot <- plot_SAI(scores, sai, time, colfunc)
myhist <- hist_SAI(scores, sai, time)

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))

dev.off()

#################################################################################
### Draw a map of the difference of SAIcc - SAIcl
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("diff_SAIcc_cl_5km_wholeNZ.data")
sai <- load("diff_SAIcc_cl_5km_wholeNZ.data")
sai <- get(sai)

# Check max/min of difference of SAI for colour scale
summary(sai$diff)

# Map SAI
png("Y:\\diff_SAIcc_SAIcl_5kmtes.png", width = 900, height = 630)


myplot <- ggplot(sai, aes_string("x", "y", fill = "diff")) + 
  geom_raster() +
  scale_fill_gradientn(colours = colfunc(30), na.value = "transparent",
                       breaks=c(-0.1, 0, 0.1),
                       limits=c(-0.1, 0.125)
  ) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

myhist <- ggplot(sai, aes_string(x = "diff")) +
  geom_histogram(data = sai) +
  xlim(-0.1, 0.125) +
  xlab("difference") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 270, vjust = 0.25),
        axis.title.y = element_text(angle = 270),
        axis.ticks.y = element_blank()
  ) +
  theme(panel.background = element_rect(fill = 'gray96'))


# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))


dev.off()



#################################################################################
### Draw a map of the difference of SAIcc - SAIcl
#################################################################################

### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

# # Load SAI values
load("diff_SAI_5km_wholeNZ.data")
sai <- load("diff_SAI_5km_wholeNZ.data")
sai <- get(sai)

# Check max/min of difference of SAI for colour scale
summary(sai$diff_cc_ll)

# Map SAI
png("Y:\\diff_SAIcc_SAIll_5km.png", width = 900, height = 630)


myplot <- ggplot(sai, aes_string("x", "y", fill = "diff_cc_ll")) + 
  geom_raster() +
  scale_fill_gradientn(colours = colfunc(30), na.value = "transparent",
                       breaks=c(-0.5, -0.25, 0, 0.25, 0.4),
                       limits=c(-0.5, 0.4)
  ) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20)
  )

myhist <- ggplot(sai, aes_string(x = "diff_cc_ll")) +
  geom_histogram(data = sai) +
  xlim(-0.5, 0.4) +
  xlab("difference") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 270, vjust = 0.25),
        axis.title.y = element_text(angle = 270),
        axis.ticks.y = element_blank()
  ) +
  theme(panel.background = element_rect(fill = 'gray96'))


# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))


dev.off()