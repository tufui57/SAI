
library(dplyr)
source(".//SAI//05_calculate_SAI_5km_differnce.R")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")


#################################################################################
### Average SAI over species occurrence records
#################################################################################

# Data of current NZ on 5km resolution
genus_name <-"Chionochloa"
# load(paste("Y:\\5th chapter SAI chapter\\raw data\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = "")) 
# 
# ### Difference of SAIcc-SAIcl
# scores.sai <- merge(sai.diff, scores, by = c("x","y"))
# spname <- colnames(scores.sai)[grepl(paste("^", genus_name, sep = ""), colnames(scores.sai))]
# 
# scores.saidiff <- cbind(scores.sai[, spname], scores.sai[, c("SAIcc", "SAIcl", "diff")])
# colnames(scores.saidiff)
# 
# aves <- sapply(spname, function(x){
#   test <- scores.saidiff[scores.saidiff[,x] == 1, ] %>% .[, c("SAIcc", "SAIcl", "diff")]
#   apply(test, 2, mean)
# }
# )
# 
# # Load range filling
# ensambleProj.name = "SAIdiff_4Mar19_ensamble"
# rangefilling <- read.csv(paste("Y://rangefilling_within_obs5km", genus_name, ensambleProj.name,".csv", sep=""))
# 
# # Merge all data
# aves2 <- as.data.frame(t(aves)) %>% cbind(rownames(.), .)
# colnames(aves2)[1] <- "spname"
# aves2$spname <- gsub("_", "\\.", aves2$spname)
# dat <- merge(rangefilling, aves2, by="spname")
# 
# # Make tags
# dat2 <- cbind(dat, makeTag_separate(dat$spname, genus_name, separate = "\\.")[,2] %>% as.data.frame)
# 
# write.csv(dat2, paste(genus_name, "_averaged_SAI_over_occ.csv", sep=""))

### LM test
dat2 <- read.csv(paste(genus_name, "_averaged_SAI_over_occ.csv", sep=""))
summary(lm(dat2$SAIcc ~ dat2$rangefilling))

### Plot
library(ggplot2)
source(".//functions//F_plotAnalysis_clade_niche.R")


for(i in c("SAIcc", "SAIcl", "diff")){
  
  myplot <- plotAnalysis(data = dat2,
                         genus_name = genus_name,
                         xv = "rangefilling", yv = i, 
                         nodeNumbercol = "tag", showStats = F,
                         ylabname = paste("Averaged", i, "over species occurrences"), 
                         xlabname = "Species range filling",
                         label.point = TRUE,label.text.size = 4
  )+
    theme(text = element_text(size=10),
          axis.text = element_text(size=10))
  
  # Turn off clipping plot areas
  gt <- ggplot_gtable(ggplot_build(myplot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)
  
  # save
  ggsave(paste("Y://rangefilling_", i, genus_name, ensambleProj.name, ".png", sep=""), plot = gt,
         width = 100, height = 80, units = 'mm')
  
  
  rm(gt)
}


