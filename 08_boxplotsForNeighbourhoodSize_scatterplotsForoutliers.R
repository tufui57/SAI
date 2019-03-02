#################################################################################
### SAIcc with different Neighbourhood size
#################################################################################

sai.list <- list()
for(i in as.character(c(20, 50, 100, 5000))){
  
  sai <- load(paste("SAI_5km_currentInCurrent_", i, "kmWindow_4var.data", sep=""))
  res <- get(sai)
  
  sai.list[[i]] <- unlist(res) 
  
}

names(sai.list) <- c(as.character(c(20, 50, 100)), "NZ")

png("SAIcc_neighbourhood.png")

boxplot(sai.list[["20"]], sai.list[["50"]], sai.list[["100"]], sai.list[["NZ"]],
        names = names(sai.list),
        xlab = "Neighbourhood size (km)", ylab = "SAIcc"
     )
  
dev.off()

#################################################################################
### SAIcc without X % of outliers
#################################################################################
# Load SAIcc
saiwith <- load("SAI_5km_currentIncurrent_5000kmWindow_4var.data")
saiwith <- get(saiwith)

sai.list <- list()
for(i in as.character(c(1, 2.5, 5))){
  sai <- load(paste("SAI_5km_currentInCurrent_5000kmWindow_4var_outlier", i, ".data", sep=""))
  res <- get(sai)
  
  sai.list[[i]] <- unlist(res)
}

names(sai.list) <- as.character(c(1, 2.5, 5))

png("SAIcc_normal_vs_outlierRemoved.png")
plot( unlist(saiwith), sai.list[["1"]],
     ylab = "SAIcc without outliers", xlab = "SAIcc with outliers",
     ylim = c(0,0.8), xlim = c(0,0.8))
points( unlist(saiwith),sai.list[["2.5"]], col="red")
points( unlist(saiwith), sai.list[["5"]],col="blue")

legend("topleft", title="Percentage of removed outlier",  pch = 1,
       c("1","2.5","5"), col = c("black","red","blue"), horiz=TRUE)

dev.off()
