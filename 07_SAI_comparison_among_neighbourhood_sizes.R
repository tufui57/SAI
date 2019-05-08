
############################################################################################################
## Visualize SAI diferences due to neighbourhood size
############################################################################################################
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

sai.c2 <- data.frame(do.call(cbind,sai.c))
colnames(sai.c2)[4] <- "NZ"

### Draw boxplots
png("SAI_current_boxplot.png")
par(las = 1,
    mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
)
boxplot(sai.c2,
        main = "SAI with 20, 50 and 100 km neighbourhood and whole NZ",
        ylab = "SAIcc",
        xlab = "Nieghbourhood size (km)",
        ylim = c(0,1)
        )
dev.off()

### Plot SAI within limited neighbourhood against SAI within NZ
png(filename = "SAIcc20km 50km NZ.png")
par(mfrow=c(2,2))
plot(sai.c2$X20, sai.c2$NZ,
     col=rgb(0,0,0,0.1),
     xlab="SAIcc 20km", ylab="SAIcc NZ"
     )
plot(sai.c2$X100, sai.c2$NZ,
     col=rgb(0,0,0,0.1),
     xlab="SAIcc 100km", ylab="SAIcc NZ"
)
dev.off()
