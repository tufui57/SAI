### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(20,50,100))){
  a <- load(paste("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_",  i, "kmWindow_4var_climateRange_of_neighbourhood.data", sep=""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}

a <- load("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_5000kmWindow_4var.data")
a <- get(a)
sai.c[[4]] <- unlist(a)

sai.c2 <- do.call(cbind, sai.c)
colnames(sai.c2)[4] <- "NZ"

### Merge Elevation data to SAI
### Load 5km data
load(".\\Scores_Acaena_landcover5km.data")

sai.dat <- cbind(scores, sai.c2)

### Elevation vs. of SAI
png(filename = "elevation_SAIcc.png")

par(mfrow = c(2,2))
plot(sai.dat$value, sai.dat$`20`, col=rgb(0,0,0,0.1), 
     xlab="Elevation (m)", ylab = "SAIcc within 20km neighbourhood")
plot(sai.dat$value, sai.dat$`50`, col=rgb(0,0,0,0.1), 
     xlab="Elevation (m)", ylab = "SAIcc within 50km neighbourhood")
plot(sai.dat$value, sai.dat$`100`, col=rgb(0,0,0,0.1),
     xlab="Elevation (m)", ylab = "SAIcc within 100km neighbourhood")
plot(sai.dat$value, sai.dat$NZ, col=rgb(0,0,0,0.1),
     xlab="Elevation (m)", ylab = "SAIcc within NZ")

dev.off()

