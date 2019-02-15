
#################################################################################
### Average SAI over species occurrence records
#################################################################################
library(dplyr)
source(".//SAI//calculate_SAI_5km_differnce.R")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

# Data of current NZ on 5km resolution
genus_name <- "Acaena"
load(paste("C:\\Users\\nomur\\Documents\\Scores_",genus_name,"_landcover5km.data",sep=""))

### Difference of SAIcc-SAIcl
scores.sai <- merge(sai.diff, scores, by = c("x","y"))
spname <- colnames(scores.sai)[grepl(paste("^",genus_name,sep=""), colnames(scores.sai))]

scores.saidiff <- cbind(scores.sai[, spname], scores.sai[, c("SAIcc", "SAIcl", "diff")])
colnames(scores.saidiff)

aves <- sapply(spname, function(x){
  test <- scores.saidiff[scores.saidiff[,x] == 1, ] %>% .[, c("SAIcc", "SAIcl", "diff")]
  apply(test, 2, mean)
}
)

# Load range filling
proj.name = "5km_15Jan19_ensamble"
rangefilling <- read.csv(paste("Y://rangefilling_", genus_name, proj.name,".csv", sep=""))

# Merge all data
aves2 <- as.data.frame(t(aves)) %>% cbind(rownames(.), .)
colnames(aves2)[1] <- "spname"
aves2$spname <- gsub("_", "\\.", aves2$spname)
dat <- merge(rangefilling, aves2, by="spname")

# Make tags
dat2 <- cbind(dat, makeTag_separate(dat$spname, genus_name, separate = "\\.")[,2] %>% as.data.frame)


### Plot
for(i in c("SAIcc", "SAIcl", "diff")){
  png(paste("rangefilling_", i, genus_name, proj.name,".png", sep=""))
  plot(dat2$rangefilling, dat2[,i],
       main = i,
       xlab = "Species range filling",
       ylab = paste("Averaged", i, "over species occurrences")
  )
  text(dat2[,i] ~ dat2$rangefilling , labels = dat2$tag, cex= 1.5)
  dev.off()
}



