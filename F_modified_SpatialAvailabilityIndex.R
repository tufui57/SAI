######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

######################################################################################
# Prepare radius size vector
######################################################################################
# Range of environmental variable * 0, 10%, ..., 90%, 100%

get_radius_size <- function(dat){
  a <- list()
  # Radius size for PCA axis 1 and 2
  a[[1]] <- ((max(dat[,"PC1"]) - min(dat[,"PC1"]) ) * (c(0, 1:10) / 10)) /2
  a[[2]] <- ((max(dat[,"PC2"]) - min(dat[,"PC2"]) ) * (c(0, 1:10) / 10)) /2

  return(a)
}

######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################

Count_cells_within_neighbourhood <- function(dat1, # data of points which are centre of search area
                                       dat2, # data of points to be searched
                                       a1, # half length (unit of the distance is the unit of coordinate) of the squire which you want to count the number of avairable secondary open cells.
                                       coordinateName # column name for climate variable in dat1 and dat2
                                       ){
  # Add cell ID to dat2
  dat1$cellID <- 1:nrow(dat1)
  
  # Count the number of dat1 cells within "a" radius neighbourhood of dat1
    dat2_in_dat1area <- lapply(dat1$cellID, function(i){
    
    # Find dat2 points within (dat1 point - a) <= dat2 points <= (dat1 point + a)
    # x axis
    datlat <- dat2[(dat2[, coordinateName] <= (dat1[i, coordinateName] + a1)), ]
    datlat2 <- datlat[(datlat[, coordinateName] >= (dat1[i, coordinateName] - a1)), ]
    
    # Name dat1 points within the neighbourhood of dat2 with cell ID of dat2
    # If "dat2cellID" has ID number, the row is within the neighbourhood of dat2
    datlat2$dat1cellID <- rep(i, nrow(datlat2))
    return(datlat2)
  }
  )

  return(dat2_in_dat1area)
}

###################################################################################################
### Count cells within neighbourhood & calculate ratio of the cells over the number of all cells
###################################################################################################

count_ratioWithinNeighbourhood <- function(dat1, # data of points to be searched
                                           dat2, # data of points which are centre of search area
                                           a, # two vectors of radius sizes
                                           coordinateName
){
  neighbours <- list()
  
  for(i in 2:length(a[[1]])){
    # Find points of a group within neighbourhood of another group of points 
    neighbours[[i]] <- Count_cells_within_neighbourhood(dat1, dat2, 
                                                        a1 = a[[1]][i], coordinateName)
    
    # Calculate percentage of area within the neighbourhood over NZ
    neighbours[[i]]$ratioWithinNeighbourhood <- nrow(neighbours[[i]][[1]]) / nrow(dat2)
    
  }
  return(neighbours)
}

######################################################################################
### Calculate Spatial availability index; AUC of ratios of cells within neighbourhood
######################################################################################

### Function to calculate AUC
# This function is copied from the following GitHub repository; https://rdrr.io/cran/MESS/src/R/auc.R
auc <-
  function(x, y, from = min(x), to = max(x), type=c("linear", "spline"), absolutearea=FALSE, ...){
    type <- match.arg(type)
    
    if (length(x) != length(y))
      stop("x and y must have the same length")
    if (length(unique(x)) < 2)
      return(NA)
    
    if (type=="linear") {
      
      ## Boost all y's to be non-negative
      if (absolutearea)
        y <- y - min(y)
      
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
      
      ## Remove the rectangle we artificially introduced above
      if (absolutearea)
        res <- res - min(y)*(max(x) - min(x))
      
    } else {
      if (absolutearea)
        myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
      else
        myfunction <- splinefun(x, y, method="natural")
      
      res <- integrate(myfunction, lower=from, upper=to)$value
    }
    
    res
  }


######################################################################################
### Calculate AUC (area under curve) doe each current grid cells
######################################################################################

calculate_auc_for_each_cell <- function(
  neighbours, # result object of the function "count_ratioWithinNeighbourhood"
  a1, # vector of radius size of PC1
  dat2 # data frame of cells which were centre of the neighbourhood
){
  for(i in 1:nrow(dat2)){
  
  # Extract ratio of cells within neighbourhood at each size of radius
  cell1 <- sapply(2:length(a1), function(j){neighbours[[j]][i,"ratioWithinNeighbourhood"]
  }
  )
  
  # AUC
  dat2[i,"AUC"] <- auc(c(0, 1:10)*0.1, c(0, cell1))
  }
  return(dat2)
}

