#################################################################################
### Calculate the difference of SAIcc - SAIcl
#################################################################################

# Load SAIcc
load("SAI_5km_currentIncurrent_1500kmWindow_4var.data")
saicc <- load("SAI_5km_currentIncurrent_1500kmWindow_4var.data")
saicc <- get(saicc)

# SAIcl

load("SAI_5km_currentInLGM_1500kmWindow_4var.data")
saicl <- load("SAI_5km_currentInLGM_1500kmWindow_4var.data")
saicl <- get(saicl)

### Load coordinates of NZ at the current and the LGM
# Data of current NZ on 5km resolution
load(".\\Scores_Acaena_landcover5km.data")

# Data of the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

### Merge the two SAIs
# Combine SAI to coordinate data
sai.cc <- cbind(scores[, c("x", "y")], unlist(saicc))
colnames(sai.cc)[3] <- "SAIcc"

# Combine SAI to coordinate data
sai.cl <- cbind(scores[, c("x", "y")], unlist(saicl))
colnames(sai.cl)[3] <- "SAIcl"

# Merge the two SAIs
sai.diff <- merge(sai.cc, sai.cl, by=c("x","y"))

### Calculate the SAI difference

sai.diff$diff <- (sai.diff$SAIcc - sai.diff$SAIcl)

save(sai.diff, file="diff_SAIcc_cl_5km_wholeNZ.data")
