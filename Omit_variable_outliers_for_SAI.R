###################################################################################################
### If outliers (1% of max/min) of climate variables were omitted, how SAI changes?
###################################################################################################

load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

coordinateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

library(ggplot2)
require(gridExtra)

his<-list()
for(i in coordinateNames){
  print(i)
  print(quantile(scores[,i], c(0.01, 0.99)))
  print(quantile(scores[,i], c(0.00, 1)))
  
  # Omit data < 1% and > 99 % quantiles
  qua <- quantile(scores[,i], c(0.01, 0.99))
  out.min <- scores[,i] <= qua[1]
  out.max <- scores[,i] >= qua[2]
  
  # To plot histograms on one panel
  scores.omit <- scores[out.min + out.max == 0,]
  scores.omit$om <- paste("1-99%", round(qua[1]), "-", round(qua[2]))
  scores$om <- paste("100%", round(min(scores[,i])), "-", round(max(scores[,i])))
  scores.hist <- rbind(scores, scores.omit)
  
  # How many % of variable range decreased?
  dec <- round(
    ((max(scores[,i])- min(scores[,i]))- ((max(scores.omit[,i])- min(scores.omit[,i]))))/((max(scores[,i])- min(scores[,i]))),
    2
  )
  
  # Plot histograms
  his[[i]] <- ggplot(scores.hist, aes_string(i, fill = "om"))+ 
    geom_histogram(alpha = 0.5, position = 'identity') +
    geom_vline(xintercept = qua[1], col="red")+
    geom_vline(xintercept = qua[2], col="red")+
    xlab(i)+
    ggtitle(paste("Decrease",dec))
  
  
}

res <- do.call("grid.arrange", c(his, ncol=1))
ggsave("1%omit_bioclim.png", plot=res)

###################################################################################################
### NULL distribution of SAIcc - difference of occurrence probabilities
###################################################################################################

# Load SAIcc
load("C:\\Users\\nomur\\Documents\\diff_SAIcc_cl_5km_wholeNZ.data")

ran <- rnorm(nrow(sai.diff), sd = 100)
plot(ran, sai.diff$SAIcc)



### Load data
binary = "prob"
genus_name="Chionochloa"

# Occurrence probability of LGM
nam <- load(paste("Y://ensemblePredictionBinary_", genus_name, "5km_15Jan19_ensamble", binary,".data", sep = ""))
cur <- get(nam)
# Occurrence probability of the current
nam.lgm <- load(paste("Y://ensemblePredictionBinary_", genus_name, "5kmLGM_15Jan19", binary,".data", sep = ""))
lgm <- get(nam.lgm)

for(i in 1:length(cur)){
  
  try({
    
    # Current occurrence probability
    dat <- as.data.frame(cbind(coordinates(cur[[i]]), values(cur[[i]])))
    colnames(dat)[3] <- "cur"
    
    # LGM occurrence probability
    dat.lgm <- as.data.frame(cbind(coordinates(lgm[[i]]), values(lgm[[i]])))
    colnames(dat.lgm)[3] <- "lgm"
    
    # Merge current and LGm data
    dat2 <- merge(dat, dat.lgm, c("x","y"))
    
    # Merge occurrence probability data and SAI data
    dat3 <- merge(dat2, sai.diff, c("x","y"))
    dat3$prob.diff <- dat3$cur - dat3$lgm
    
    ran <- rnorm(nrow(sai.diff), mean = mean(dat3$prob.diff), sd = sd(dat3$prob.diff))
    plot(ran, sai.diff$diff)
    
    points(dat3$prob.diff, sai.diff$diff, col="red")
    
    
    
  }
)
}