#' \code{assessSpatialBias}
#'
#' This function calculates how far the data deviates from a random distribution in geographic space. It calculates
#' a nearest neighbour index, defined as the mean of the nearest neighbour distances of the emprical data divided by
#' the mean of the neares neighbour distances of a random sample. The user can choose how many random samples to average
#' over, which should be larger where there are fewer data. This is because the random samples are generated in equal number to
#' the data. Where the number of samples is > 1, the estimates come with a measures of uncertainty (5th and 95th percentiles). 
#' The index is calculated for each of n user-specified time periods.
#' @param dat string. A data.frame containing columns for species name, x coordinates, y coordinates, spatialUncertainty, year and an identifier (used to group the data - heuristic will be calculated for each group). 
#' @param species Character string. column name in dat giving species names.
#' @param x string. Column name in dat giving x coordinates. Any coordinate and spatial reference systems are permitted.
#' @param y string. Column name in dat giving y coordinates. Any coordinate and spatial reference systems are permitted.
#' @param year string. Column name in dat giving years.
#' @param spatialUncertainty String. Column name in dat giving uncertainty associated with x and y. Any units are permitted. 
#' @param identifier String. Column name in dat giving record "identifiers". Identifiers are used to group the data; heuristics will be calculated separately for each group.
#' @param periods Numeric. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param nSamps Logical. How many iterations of random samples to use for comparison of empirical NN index with random NN index.
#' @param mask String. A raster object used to indicate the study region over which the random distribution should be generated. In most cases this will be a single raster layer. However, where the identifier 
#'             field is used to subset the data spatially (e.g. splitting it by country, continent, etc.) then mask should be a raster stack with n layers, where n is equal to the number of levels in the identifier field. 
#'             mask may have fewer layers than the number of levels in identifier field when identifier refers to spatial subsets, but any rows in dat with an identifier for which there is no layer 
#'             in mask will be dropped. If nlayers(mask) > 1, i.e. if the identifier field is used to subset the data spatially, then names(mask) should match the names in the identifier field.
#'        Must be NA where points are not to be generated, and numeric where they may be generated. For example, this could be a map of worldclim climate data, cropped to the study region.
#' @param degrade Logical. Whether or not to remove duplicated coordinates from the data. Coordinates are not considered to be duplicated if they are from
#'        different \code{periods}.
#' @param maxSpatUncertainty Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.
#' @seealso \code{\link{assessSpatialCov}} which maps your data in geographical space.
#' @return A list with two elements: a ggplot2 object and the data underpinning the plot.
#' @export

assessSpatialBias <- function(dat, 
                              species,
                              x,
                              y,
                              year,
                              spatialUncertainty,
                              identifier,
                              periods, 
                              mask, 
                              nSamps = 50, 
                              degrade = TRUE, 
                              maxSpatUncertainty = NULL) {
  
  if (any(!(c(species, x, y, year, spatialUncertainty, identifier) %in% colnames(dat)))) stop("You have specified columns that don't exist in dat.")
  
  dat <- createData(data = dat,
                    species,
                    x,
                    y,
                    year,
                    spatialUncertainty,
                    identifier)  
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (!is.null(maxSpatUncertainty)) dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]

  if (nrow(dat) == 0) stop("No records with with spatialUncertainty < maxSpatUncertainty")
  
  if (raster::nlayers(mask) > 1) {
    
    print("You have more than one layer in mask - this should only be the case if your identifier field denotes spatial subsets of your data.")
    
    if (!any(unique(dat$identifier) %in% names(mask))) stop("No names of layers in mask match levels in the identifier field. Make sure the names in mask correspond to identifier where identifier denotes spatial subsets; otherwise, you should not have multiple layers in mask.")
      
    if (any(!(unique(dat$identifier) %in% names(mask)))) {
      
      
      warning("Some levels of identifier are not present in names(mask). Dropping data with an identifier for which no mask layer has been provided.")
    
      dat <- dat[-which(!dat$identifier %in% names(mask)), ]
      
    }
    
  }

  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }

  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], i, dat$Period)
    
  }


if (degrade == TRUE & any(duplicated(dat[, c("x", "y", "identifier", "Period")]))) {

  dat <- dat[!duplicated(dat[, c("x", "y", "identifier", "Period")]), ]

}

  dat <- dat[order(dat$year), ]
  
  if (any(is.na(dat$year))) dat <- dat[-which(is.na(dat$year)), ]

  for (i in unique(dat$identifier)) {

    if (raster::nlayers(mask) > 1) { ## ifelse won't work for some reason...
      
      domain <- mask[[i]]
      
    } else {
      
      domain <- mask
      
    }

    index <- lapply(1:length(periods),
                    function(y) {
   
                      pDat <- dat[dat$Period == y & dat$identifier == i, ]
                      
                      if (nrow(pDat) > 2) {
                        
                        if (nrow(pDat) < 100) warning(paste("Fewer than 100 records in period", y, "for", i, ". View this result with caution."))
                        
                        empDist <- spatstat.geom::nndist(X = pDat$x, Y = pDat$y, k = 1)
                        
                        empMean <- mean(empDist)
                        
                        randomSamp <- lapply(1:nSamps, 
                                             function(x) {
                                               
                                               ran <- raster::sampleRandom(domain,
                                                                           size = ifelse(nrow(pDat) <= raster::ncell(domain), nrow(pDat), raster::ncell(domain))
                                                                           xy = T)
                                               
                                               dist <- spatstat.geom::nndist(X = ran[,1], Y = ran[,2], k = 1) 
                                               
                                               NN <- mean(dist)
                                               
                                             })
                        
                        randomSamp <- do.call("c", randomSamp)
                        
                        randomSamp <- randomSamp[is.finite(randomSamp)]
                        
                        indDist <- empMean / randomSamp
                        
                        meanInd <- mean(indDist)
                        
                        upper <- as.numeric(stats::quantile(indDist, prob = 0.95))
                        
                        lower <- as.numeric(stats::quantile(indDist, prob = 0.05))
                        
                      } else {
                        
                        warning(paste("Fewer than 2 records in period", y, "for", i, ". Index will not be calculated for this period/ identifier combination."))
                        
                        meanInd <- NA
                        
                        upper <- NA
                        
                        lower <- NA
                        
                      }
                      
                      
                      out <- data.frame(mean = meanInd,
                                        upper = upper,
                                        lower = lower,
                                        Period = paste(y),
                                        identifier = paste(i))
                      
                    }
    ) 
    
    assign(paste0("index_", i), do.call("rbind", index))                  
    
  }
  
  
  data <- lapply(unique(dat$identifier),
                 function(x) { get(paste0("index_", x))})
  
  data <- do.call("rbind", data)
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = is.numeric(Period), y = mean, group = identifier, ymin = lower, ymax = upper, fill = identifier, colour = identifier)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(alpha = 0.5) +
    ggplot2::theme_linedraw() + 
    ggplot2::ylab("Nearest neighbour index") +
    ggplot2::labs(fill = "") +
    ggplot2::xlab("Period") +
    ggplot2::guides(colour = FALSE)

  return(list(data = data, 
              plot = p))
  
  
}



