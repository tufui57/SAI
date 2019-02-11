###################################################################################################
### Correlation between SAI depending on neighbourhood window sizes
###################################################################################################

library(dplyr)

### Load SAI data
filenames <- list.files(getwd())

sais <- filenames[grepl("SAI_5km_currentInCurrent|SAI_5km_LGMInLGM", filenames)] %>% 
  lapply(., function(x){
    name <- load(x)
    dat <- get(name)
    dat2 <- unlist(dat)
    return(dat2)
    })

dat <- as.data.frame(do.call(cbind, sais[1:3]))
dat.lgm <- as.data.frame(do.call(cbind, sais[4:6]))

colnames(dat) <- paste(rep("SAI",3), rep("current",3), c(20, 50, 100), sep="_")
colnames(dat.lgm) <- paste(rep("SAI",3), rep("LGM",3), c(20, 50, 100), sep="_")

### Load current 5km data
load(".\\Scores_Acaena_landcover5km.data")
dat2 <- cbind(scores[,c("x","y")], dat)

### Load LGM mainland data
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
dat.lgm2 <- cbind(scores.lgm[,c("x","y")], dat.lgm)


### Correlation between SAI
summary(lm(dat$SAI_current_20 ~ dat$SAI_current_50))
summary(lm(dat$SAI_current_50 ~ dat$SAI_current_100))

summary(lm(dat.lgm$SAI_LGM_20 ~ dat.lgm$SAI_LGM_50))
summary(lm(dat.lgm$SAI_LGM_50 ~ dat.lgm$SAI_LGM_100))

### Correlation between SAI of LGM and current

dat3 <- merge(dat2, dat.lgm2, by = c("x","y"))
plot(dat3$SAI_current_20, dat3$SAI_LGM_20)

dat3$dif.sai20 <-  dat3$SAI_current_20 - dat3$SAI_LGM_20

write.csv(dat3, file = "SAI_current_LGM_dif.csv")
