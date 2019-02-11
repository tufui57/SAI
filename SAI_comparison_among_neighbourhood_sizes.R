
############################################################################################################
## Visualize SAI diferences due to neighbourhood size
############################################################################################################
### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(10,20,50,100,1500))){
  a <- load(paste("SAI_5km_currentInCurrent_",  i, "kmWindow_4var.data", sep = ""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}


sai.lgm <- list()
for(i in as.character(c(10,20,50,100,1500))){
  a <- load(paste("SAI_5km_LGMInLGM_",  i, "kmWindow_4var.data", sep = ""))
  a <- get(a)
  sai.lgm[[i]] <- unlist(a)
}

sai.c2 <- do.call(cbind,sai.c)
colnames(sai.c2)[5] <- "NZ"
sai.lgm2 <- do.call(cbind,sai.lgm)
colnames(sai.lgm2)[5] <- "NZ"

### Draw boxplots
png("SAI_current_boxplot.png")
par(las = 1,
    mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
)
boxplot(sai.c2,
        main = "SAI with 10, 20, 50 and 100 km neighbourhood and whole NZ",
        ylab = "SAIcc",
        xlab = "Nieghbourhood size (km)",
        ylim = c(0,1)
        )
dev.off()

png("SAI_lgm_boxplot.png")
par(las = 1,
    mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
)
boxplot(sai.lgm2,
        main = "SAI with 10, 20, 50 and 100 km neighbourhood and whole NZ",
        ylab = "SAIll",
        xlab = "Nieghbourhood size (km)",
        ylim = c(0,1)
)
dev.off()


# The following figure looks wrong but I have no script how I obtained this data
# ############################################################################################################
# ## Visualize SAI diferences due to neighbourhood size
# ############################################################################################################
# 
# ### Load SAI with different neighbourhood sizes
# sai <- read.csv("SAI_current_LGM_dif.csv")
# 
# png("SAI_boxplot.png")
# par(las = 2,
#     mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
# )
# boxplot(sai[, c(-1,-2,-3, -ncol(sai))],
#         main = "SAI with 20, 50 and 100 km neighbourhood",
#         ylab = "SAI")
# dev.off()
