library(raster)
require(ggplot2)
require(reshape2)
library(gridExtra)

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

swiss <- read.csv("Y://swiss_all_climate_2.csv")

# Region 1
p = swiss[(swiss$LONG <= 7.65) & (swiss$LONG >= 7.4),]
reg1 <- p[(p$LAT <= 47.3) & (p$LAT >= 47),]

# Region 2
p = swiss[(swiss$LONG <= 7.4) & (swiss$LONG >= 7.15),]
reg2 <- p[(p$LAT <= 46.55) & (p$LAT >= 46.2),]

# Region 3
p = swiss[(swiss$LONG <= 8.9) & (swiss$LONG >= 8.65),]
reg3 <- p[(p$LAT <= 46.75) & (p$LAT >= 46.45),]

#################################################################################
### DraW
#################################################################################

figure_sai <- function(i,j, plot = TRUE){
  a <- load(paste("SAIcc_of_swissRegion", i, "_in_region", j, "_p.data", sep = ""))
  a <- get(a)
  
  if(i==1){
    region = reg1
  }
  if(i==2){
    region = reg2
  }
  if(i==3){
    region = reg3
  }
  
  myplot <- plot_SAI(scores = region, sai = a, nameOfsai = "SAIcc", coordinateNames = c("LONG", "LAT"), colfunc) +
    ggtitle(paste("EP of region", i, "in region", j))
  
  if(plot==T){
  png(paste("Y://ep.swissRegion", i, "_in_region", j, ".png", sep=""), width = 900, height = 630)
  
  print(myplot)
  
  dev.off()    
  }else{
    myplot <- plot_SAI(scores = region, sai = a, nameOfsai = "SAIcc", coordinateNames = c("LONG", "LAT"), colfunc)+ theme(legend.position="none")
      
    return(myplot)
  }

}

map31 <- figure_sai(3,1, plot=F)

map32 <- figure_sai(3,2, plot=F)

map12 <- figure_sai(1,2, plot=F)

map13 <- figure_sai(1,3, plot=F)

map21 <- figure_sai(2,1, plot=F)

map23 <- figure_sai(2,3, plot=F)



load(".//SAIcc_swissRegion1_25km_neighbourhood_ref.data")
load(".//SAIcc_swissRegion2_25km_neighbourhood_ref.data")
load(".//SAIcc_swissRegion3_25km_neighbourhood_ref.data")

map1 <- plot_SAI(scores = reg1, sai = sai.swiss, nameOfsai = "SAIcc", coordinateNames = c("LONG", "LAT"), colfunc)+ theme(legend.position="none")
map2 <- plot_SAI(scores = reg2, sai = sai.swiss2, nameOfsai = "SAIcc", coordinateNames = c("LONG", "LAT"), colfunc)+ theme(legend.position="none")
map3 <- plot_SAI(scores = reg3, sai = sai.swiss3, nameOfsai = "SAIcc", coordinateNames = c("LONG", "LAT"), colfunc)+ theme(legend.position="none")

png("Y://ep.swissRegion.png", width = 1500, height = 1500)

grid.arrange(map1, map12, map13, 
             map21, map2, map23,
             map31, map32, map3,
             nrow=3)
dev.off()



# png("Y://sai.swissRegion2_in_region1.png", width = 900, height = 630)
# ggplot(sai.dat) + 
#   geom_raster(aes_string(coordinateNames[1], coordinateNames[2], fill="SAIcc")) +
#   theme(axis.text        = element_blank(),
#         axis.ticks       = element_blank(),
#         axis.title       = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size=20)
#   )
# 
# dev.off()

