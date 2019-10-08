########################################
library(ggplot2)
library(raster)
library(dplyr)

load("Y://swiss_climate_clipped.data")

map_climate <- function(dat, i){
  
  dat2 <- cbind(coordinates(dat[[i]]), values(dat[[i]])) %>%  as.data.frame
  colnames(dat2) <- c("Longitude", "Latitude", "val")
  
  ggplot(data = dat2) +
    geom_raster(aes_string(x = "Longitude", y = "Latitude", fill = "val")) +
    scale_fill_gradientn(colours = c("blue", "white", "red"), 
                         guide = "colorbar", na.value = "white") +
    geom_rect(data = d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
              alpha = 0, inherit.aes = FALSE, size = 1) +
    geom_rect(data = d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
              alpha = 0, inherit.aes = FALSE, linetype = 2, size = 1) +
    theme(panel.background = element_blank(),
          panel.border= element_rect(color="black", fill = NA),
          text = element_text(size=25)
    )
}

dat <- clipped.swiss2


# Add squares showing the example regions on the map
d <- data.frame(x1 = c(7.4,7.15,8.65), x2 = c(7.65,7.4,8.9), y1 = c(47,46.225,46.45), y2 = c(47.3,46.525,46.75))
d2 <- data.frame(cbind(d$x1 - 0.2, d$x2 + 0.2, d$y1 - 0.2, d$y2 + 0.2))
colnames(d2) <- colnames(d)


# Map
png("Y:\\swiss_map_tave68.png", width = 1300, height = 630)

map_climate(clipped.swiss2, i =2)

dev.off()

# Map
png("Y:\\swiss_map_tave122.png", width = 1300, height = 630)

map_climate(clipped.swiss2, i =3)

dev.off()


# Map
i = 4
dat2 <- cbind(coordinates(dat[[i]]), values(dat[[i]])) %>%  as.data.frame
colnames(dat2) <- c("Longitude", "Latitude", "val")

png("Y:\\swiss_map_prec49.png", width = 1300, height = 630)

ggplot(data = dat2) +
  geom_raster(aes_string(x = "Longitude", y = "Latitude", fill = "val")) +
  scale_fill_gradientn(colours = c("white", "blue"), 
                       guide = "colorbar", na.value = "white") +
  geom_rect(data = d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
            alpha = 0, inherit.aes = FALSE, size = 1) +
  geom_rect(data = d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
            alpha = 0, inherit.aes = FALSE, linetype = 2, size = 1) +
  theme(panel.background = element_blank(),
        panel.border= element_rect(color="black", fill = NA),
        text = element_text(size=25)
  )
dev.off()

# Map
i = 5
dat2 <- cbind(coordinates(dat[[i]]), values(dat[[i]])) %>%  as.data.frame
colnames(dat2) <- c("Longitude", "Latitude", "val")

png("Y:\\swiss_map_prec102.png", width = 1300, height = 630)

ggplot(data = dat2) +
  geom_raster(aes_string(x = "Longitude", y = "Latitude", fill = "val")) +
  scale_fill_gradientn(colours =c("white", "blue"), 
                       guide = "colorbar", na.value = "white") +
  geom_rect(data = d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
            alpha = 0, inherit.aes = FALSE, size = 1) +
  geom_rect(data = d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = "black", 
            alpha = 0, inherit.aes = FALSE, linetype = 2, size = 1) +
  theme(panel.background = element_blank(),
        panel.border= element_rect(color="black", fill = NA),
        text = element_text(size=25)
  )

dev.off()
