######################################################################################
### Calculate spatial availability index on 2 dimentional ordination space 
######################################################################################

######################################################################################
# Prepare radius size vector
######################################################################################
# Range of environmental variable * 0, 10%, ..., 90%, 100%

a1 <- ((max(dat2[,"PC1"]) - min(dat2[,"PC1"]) ) * (c(0, 1:10) / 10)) /2
a2 <- ((max(dat2[,"PC2"]) - min(dat2[,"PC2"]) ) * (c(0, 1:10) / 10)) /2

######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################

Count_cells_within_neighbourhood <- function(dat1, # data of points to be searched
                                       dat2, # data of points which are centre of search area
                                       a1, a2, # half length (unit of the distance is the unit of coordinate) of the squire which you want to count the number of avairable secondary open cells.
                                       coordinateNames # column names for coordinates in dat1 and dat2
                                       ){
  # Add cell ID to dat2
  dat2$cellID <- 1:nrow(dat2)
  
  # Count the number of secondary open cells within squire of the distance "a"
  
    dat1_in_dat2area <- lapply(dat2$cellID, function(i){
    
    # Find dat1 points within dat2 point -a <= dat1 point <= dat2 point + a 
    datlat <- dat1[(dat1[, coordinateNames[2]] <= (dat2[i, coordinateNames[2]] + a2) & dat1[, coordinateNames[2]] >= (dat2[i, coordinateNames[2]] - a2)), ]
    datlatlon <- datlat[(datlat[, coordinateNames[1]] <= (dat2[i, coordinateNames[1]] + a1) & datlat[, coordinateNames[1]] >= (dat2[i, coordinateNames[1]] - a1)), ]
    
    
    # Name the group of dat1 points with cell ID of dat2
    # If the "dat2cellID" of a dat2 row is 1, the point of the dat2 row is within the neighbourhood of row 1 of dat2
    datlatlon$dat2cellID <- rep(i, nrow(datlatlon))
    return(datlatlon)
  }
  )
  
  # Count the number of cells within neighbourhood
  dat2$dat1cellnumber <- sapply(dat1_in_dat2area, nrow)
  
  return(dat2)
}

###################################################################################################
### Count cells within neighbourhood & calculate ratio of the cells over the number of all cells
###################################################################################################

count_ratioWithinNeighbourhood <- function(dat1, dat2,
                                           coordinateNames
){
  neighbours <- list()
  
  for(i in 2:length(a1)){
    neighbours[[i]] <- Count_cells_within_neighbourhood(dat1, dat2, 
                                                        a1 = a1[i], a2 = a2[i], coordinateNames)
    
    # Calculate percentage of area within the neighbourhood over NZ
    neighbours[[i]]$ratioWithinNeighbourhood <- neighbours[[i]]$dat1cellnumber / nrow(dat1)
    
  }
  return(neighbours)
}

######################################################################################
### Calculate Spatial availability index; AUC of ratios of cells within neighbourhood
######################################################################################

### Function to calculate AUC
# This function is copied from the following GitHub repository
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

