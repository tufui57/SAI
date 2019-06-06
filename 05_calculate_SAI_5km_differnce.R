#################################################################################
### Calculate the difference of SAIcc - SAIcl
#################################################################################

# Load SAIcc
load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentInCurrent_5000kmWindow_4var.data")
saicc <- load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentInCurrent_5000kmWindow_4var.data")
saicc <- get(saicc)

# SAIcl
load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentInLGM_5000kmWindow_4var.data")
saicl <- load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentInLGM_5000kmWindow_4var.data")
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

### Calculate the difference SAIcc - SAIcl 
sai.diff$diff <- (sai.diff$SAIcc - sai.diff$SAIcl)

# #################################################################################
# ### Calculate the difference SAIcc - SAIll
# #################################################################################
# 
# load("SAI_5km_LGMInLGM_5000kmWindow_4var.data")
# saill <- load("SAI_5km_LGMInLGM_5000kmWindow_4var.data")
# saill <- get(saill)
# 
# sai.ll <- cbind(scores.lgm[, c("x", "y")], unlist(saill))
# colnames(sai.ll)[3] <- "SAIll"
# 
# # Merge the two SAIs
# sai.diff <- merge(sai.diff, sai.ll, by=c("x","y"))
# 
# sai.diff$diff_cc_ll <- (sai.diff$SAIcc - sai.diff$SAIll)

save(sai.diff, file="diff_SAIcc_cl_5km.data")
