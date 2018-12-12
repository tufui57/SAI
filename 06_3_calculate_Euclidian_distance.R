###################################################################################################
### Calculate Euclidean similarity (ES)
###################################################################################################
# The calculation takes long. So, run for every 10000 rows (1 row takes 3 sec).
res<- cbind(100001:110000, unlist(similarity))

write.csv(res, file="es110000.csv")

###################################################################################################
### Bind all ES data
###################################################################################################

res<-list()
res <- lapply(paste("es", seq(10000, 90000, 10000), ".csv", sep=""), read.csv)

res[[10]] <- read.csv("es100000.csv")
res[[11]] <- read.csv("es110000.csv")
res[[12]] <- read.csv("es120000.csv")
res[[13]] <- read.csv("es130000.csv")
res[[14]] <- read.csv("es140000.csv")
res[[15]] <- read.csv("es150000.csv")
res2 <- do.call(rbind, res)

write.csv(res2, file="es.csv")

# Bind ES to other indices
SAI<-read.csv(".\\SAI_SI.csv")
res3 <- cbind(SAI, res2)

colnames(res3)[ncol(res3)] <- "euclidian"

write.csv(res3, file="SI_4indices.csv")
