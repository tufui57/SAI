library(sp)
library(rgdal) 
library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)

############################## SAI for Swiss data ##################################

swiss <- read.table("Y://swiss_all_climate_2.txt", header = T)
swiss <- data.frame(swiss)

### Extract example areas
# Region 1
p = swiss[(swiss$LONG <= 7.65) & (swiss$LONG >= 7.4),]
reg1 <- p[(p$LAT <= 47.3) & (p$LAT >= 47),]


#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

res <- LongLatToUTM(reg1$LONG, reg1$LAT, 33)
reg1[c("LATm","LONGm")] <- res[, c("Y","X")]

climateNames = colnames(reg1)[5:8]

sai.swiss <- calc_SAI(data1 = reg1,
                  data2 = reg1,
                  climateNames = climateNames,
                  coordinateNames = c("LATm","LONGm"),
                  no.neighbourhood = T
)
# Save
save(sai.swiss, file = "SAIcc_swissRegion1.data")

mean(sai.swiss)



# Region 2
p = swiss[(swiss$LONG <= 7.4) & (swiss$LONG >= 7.15),]
reg2 <- p[(p$LAT <= 46.55) & (p$LAT >= 46.2),]

res <- LongLatToUTM(reg2$LONG, reg2$LAT, 33)
reg2[c("LATm","LONGm")] <- res[, c("Y","X")]

climateNames = colnames(reg2)[5:8]

sai.swiss2 <- calc_SAI(data1 = reg2,
                      data2 = reg2,
                      climateNames = climateNames,
                      coordinateNames = c("LAT","LONG"),
                      no.neighbourhood = T
)
# Save
save(sai.swiss2, file = "SAIcc_swissRegion2_degree.data")





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

hist_SAI <- function(scores, sai, nameOfsai,
                     coordinateNames # the order must be c("x", "y")
                     ){
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, coordinateNames], unlist(sai))
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


#################################################################################
### DraW MAPS 
#################################################################################

load(".//SAIcc_swissRegion1.data")

png("Y://sai.swissRegion1.png", width = 900, height = 630)
myplot <- plot_SAI(scores = reg2, sai = sai.swiss2, nameOfsai = "SAIcc", coordinateNames = c("LONG", "LAT"), colfunc)
myhist <- hist_SAI(reg2, sai.swiss2, nameOfsai = "SAIcc", coordinateNames = c("LONG","LAT"))

# Plot in multiple panels
grid.arrange(myplot, myhist,
             ncol = 2, nrow = 1, widths = c(2, 1))
dev.off()



# Combine SAI to coordinate data
sai.dat <- cbind(reg2[, coordinateNames], unlist(sai.swiss2))
colnames(sai.dat)[3] <- "SAIcc"

# myplot <- 
  ggplot(sai.dat) + 
  geom_raster(aes_string(coordinateNames[1], coordinateNames[2], fill="SAIcc")) +
    scale_fill_manual(values = terrain.colors(100))
  # scale_fill_manual(nameOfsai, values=setNames(colfunc(11), levels(sai.dat$SAIcc)), na.value = "transparent"
  # ) +
  # theme(axis.text        = element_blank(),
  #       axis.ticks       = element_blank(),
  #       axis.title       = element_blank(),
  #       panel.background = element_blank(),
  #       text = element_text(size=20)
  )



