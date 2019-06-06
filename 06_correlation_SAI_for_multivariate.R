###################################################################################################
### Correlation matrix among SAIs within 4 neighbourhood sizes / SAIcc, SAIcl and SAIll
###################################################################################################

library(dplyr)

### Load SAIcc, SAIcl, SAIll
sais <- paste("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_", c("currentInCurrent", "LGMInLGM", "currentInLGM"), "_5000kmWindow_4var.data", sep="") %>% 
  lapply(., function(x){
    name <- load(x)
    dat <- get(name)
    dat2 <- unlist(dat)
    return(dat2)
    })
# Merge the list
dat <- as.data.frame(do.call(cbind, sais[c(1,3)]))
dat.lgm <- as.data.frame(do.call(cbind, sais[2]))
colnames(dat) <- c("SAIcc","SAIcl")
colnames(dat.lgm) <- "SAIll"

### Load SAI with different neighbourhood sizes
sai.c <- list()
for(i in as.character(c(20,50,100))){
  a <- load(paste("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_",  i, "kmWindow_4var_climateRange_of_neighbourhood.data", sep=""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}
a <- load("Y://5th chapter SAI chapter//meta data//SAI_5km_currentInCurrent_5000kmWindow_4var.data")
a <- get(a)
sai.c[["NZ"]] <- unlist(a)

# Merge the list
dat.nei <- as.data.frame(do.call(cbind, sai.c))

colnames(dat.nei) <- c("SAI20","SAI50","SAI100","NZ")

############################################################################################################
## Correlation matrix for SAIs within 4 diferent neighbourhood size
############################################################################################################

# Plot
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.sca <- function(x, y, ...) {
  points(x, y, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25),
         cex=0.05)
}

png("Y://SAI_comparison_4neighbourhoods.png")
pairs(dat.nei,
      diag.panel=panel.hist, upper.panel = panel.cor,lower.panel = panel.sca
)
dev.off()
############################################################################################################
## LM for SAIcc and SAIcl
############################################################################################################

summary(lm(dat))
