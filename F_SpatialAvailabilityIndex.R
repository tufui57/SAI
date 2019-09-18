######################################################################################
### Calculate spatial availability index (SAI) on 2 dimentional ordination space 
######################################################################################

### Steps for SAI calculation
# 1. Prepare a x a km2 neighbourhood square for a target grid cell. 
# For the cells along coastlines, the neighbourhood can overlap with the sea.
# 2. Prepare 11 climate range breadth steps (0, 10%, ..., 90%, 100% of the total range breadth of cells within the neighbourhood).
# At 100% range step, the climate range, (climate condition of target cell - the range) - (climate condition of target cell + the range), 
# must cover all climate conditions of any grid cells in NZ.
# 3. Identify the cells within the neighbourhood, cells with analogous climates, for each climate range step and variable.
# 6. Calculate the proportion of the cells with analogous climates over all grid cells in NZ.
# 7. Draw a graph which its x axis is k (0, 10%, ..., 90%, 100%) and the y axis is the proportion.
# 8. Calculate Area Under the Curve. The AUC is SAI for the target grid cell.

######################################################################################
# Prepare radius size vector
######################################################################################
# Breadth of environmental variable * 0, 10%, ..., 90%, 100%

get_radius_size <- function(dat, coordinateName){
  
  # NOTE the radius size MUST NOT be half of environmental variables, meaning a / 2 is not the radius you need!
  a <- ((max(dat[,coordinateName]) - min(dat[,coordinateName]) ) * (c(0, 1:10) / 10)) 

  return(a)
}

######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################

Count_cells_within_neighbourhood <- function(p, # a point at the centre of search area
                                             dat2, # data of points to be searched
                                             a1, # half length of the variable range/neighbourhood radius within which you want to count the number of points.
                                             coordinateName # column name for climate variable in p and dat2
){
  # Find dat2 points within ("p" point - a) <= dat2 points <= ("p" point + a)
  dat.plus <- dat2[(dat2[, coordinateName] <= (p[, coordinateName] + a1)), ]
  dat.minus <- dat.plus[(dat.plus[, coordinateName] >= (p[, coordinateName] - a1)), ]
  
  return(dat.minus)
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


###################################################################################################
### Count cells within overlapped neighbourhood among multiple variables
###################################################################################################

cells_within_neighbourhood_multivariate <- function(p, # a point at the centre of search area
                                             dat2, # data of points to be searched
                                             ranges, # result of get_radius_size()
                                             twicerange = TRUE, # Logical. TRUE; 100% range covers all cells from any cells. FALSE; some cells might not be covered by 100% range.
                                             coordinateNames # column name for climate variable in p and dat2
){

  neighbours.size <- list()
  
  for(j in 2:length(ranges[[1]])){
    
    neighbours <- list()
    
    # Get cells within (j -1)*10 % neighbourhood of variable 1
    neighbours[[1]] <- Count_cells_within_neighbourhood(p, dat2,
                                                        a1 = ifelse(twicerange, ranges[[1]][j], ranges[[1]][j] / 2), 
                                                        coordinateNames[1])
    for(i in 1:(length(coordinateNames)-1)){
      
      # Get cells within (j -1)*10 % neighbourhood of variable i+1
      neighbours[[i+1]] <- Count_cells_within_neighbourhood(p, neighbours[[i]],
                                                            a1 =  ifelse(twicerange,  ranges[[i+1]][j], ranges[[i+1]][j] / 2), 
                                                            coordinateNames[i+1])
    }
    # Cells within (j -1)*10 % neighbourhood of all the variables
    neighbours.size[[j]] <- neighbours[[length(coordinateNames)]]
  }
  
  return(neighbours.size)
}

# # Check the number of cells within each neighbourhood
# sapply(neighbours.size, nrow)
# 
# ranges[coordinateNames[1:2]]
# 
# plot(dat2[, coordinateNames[1:2]])
# points(neighbours.size[[2]][, coordinateNames[1:2]], col="lightblue")
# points(neighbours.size[[3]][, coordinateNames[1:2]], col="green")
# points(neighbours.size[[11]][, coordinateNames[1:2]], col="pink")
# points(p[, coordinateNames[1:2]], col="red")

###################################################################################################
### SAI
###################################################################################################
# SAI of the time at the time (e.g. SAI of the current climate at the current time)

SAI <- function(p, # a point at the centre of search area
                dat2, # data of points to be searched
                ranges, # result of get_radius_size()
                twicerange,
                coordinateNames # column name for climate variable in p and dat2
){
  
  neighbours.size <- cells_within_neighbourhood_multivariate(p, # a point at the centre of search area
                                          dat2, # data of points to be searched
                                          ranges, # result of get_radius_size()
                                          twicerange,
                                          coordinateNames # column name for climate variable in p and dat2
  )
  
  # Calculate percentage of area within the neighbourhood over NZ
  ratio <- lapply(2:length(ranges[[1]]),
                  function(j){
                    # Find points of a group within neighbourhood of another group of points 
                    nrow(neighbours.size[[j]]) / nrow(dat2)
                  }
  )
  
  ### Calculate AUC (area under curve) for each current grid cells
  res <- auc(c(0, 1:10)*0.1, c(0, ratio), type="spline")
  
  return(res)
}

###################################################################################################
### Calculate variable ranges without X % outliers
###################################################################################################

ranges_without_outliers <- function(dat, 
                                    coordinateName, 
                                    outlierPercent){
  
  # Omit data < 1% and > 99 % quantiles
  qua <- quantile(dat[,coordinateName], c(outlierPercent * 0.01, 1 - outlierPercent * 0.01))
  out.min <- dat[,coordinateName] <= qua[1]
  out.max <- dat[,coordinateName] >= qua[2]
  
  # Create radius
  dat.omit <- dat[out.min + out.max == 0,]
  
  # How many % of variable range decreased?
  dec <- round(
    ((max(dat[,coordinateName])- min(dat[,coordinateName]))- ((max(dat.omit[,coordinateName])- min(dat.omit[,coordinateName]))))/((max(dat[,coordinateName])- min(dat[,coordinateName]))),
    2
  )
  
  print(dec)
  
  res <- get_radius_size(dat.omit, coordinateName)
  return(res)
}

###################################################################################################
### Function to replace outlier values with min or max values of the variables
###################################################################################################

check_outliers <- function(p, dat, 
                           coordinateName, 
                           outlierPercent){
  
  # Omit data < 1% and > 99 % quantiles
  qua <- quantile(dat[,coordinateName], c(outlierPercent * 0.01, 1 - outlierPercent * 0.01))
  out.min <- dat[,coordinateName] <= qua[1]
  out.max <- dat[,coordinateName] >= qua[2]
  
  ### Replace outliers with min or max values of the variables
  # If the value is smaller than min
  if(sum(p[, coordinateName] == dat[out.min, coordinateName]) > 0){
    print(paste(coordinateName, "< min"))
    p[, coordinateName] <- qua[1]
  }
  # If the value is bigger than max
  if(sum(p[, coordinateName] == dat[out.max, coordinateName]) > 0){
    print(paste(coordinateName, "> max"))
    p[, coordinateName] <- qua[2]
  }
  
  return(p[,coordinateName])
}



