#' \code{assessSpatialBias}
#'
#' This function calculates how far the data deviates from a random distribution in geographic space. It calculates
#' a nearest neighbour index, defined as the mean of the nearest neighbour distances of the emprical data divided by
#' the mean of the neares neighbour distances of a random sample. The user can choose how many random samples to average
#' over, which should be larger where there are fewer data. This is because the random samples are generated in equal number to
#' the data. Where the number of samples is > 1, the estimates come with a measures of uncertainty (5th and 95th percentiles). 
#' The index is calculated for each of n user-specified time periods.
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and spatial uncertainty.
#' @param periods Numeric. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param nSamps Logical. How many iterations of random samples to use for comparison of empirical NN index with random NN index.
#' @param mask String. A raster object used to indicate the study region over which the random distribution should be generated. 
#'        Must be NA where points are not to be generated, and numeric where they may be generated. For example, this could be a map of worldclim climate data, cropped to the study region.
#' @param degrade Logical. Whether or not to remove duplicated coordinates from the data. Coordinates are not considered to be duplicated if they are from
#'        different \code{periods}.
#' @return A list with two elements if filter = FALSE and three elements if filter = TRUE. The elements are 1) data (summary of spatial uncertainty),
#'         2) a ggplot object and 3) the input data with the user-defined spatialUncertainty filter applied.
#' @export
#' @examples

assessSpatialBias <- function(dat, periods, mask, nSamps = 50, degrade = TRUE) {
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }


if (degrade == TRUE & any(duplicated(dat[, c("x", "y", "identifier", "Period")]))) {

  dat <- dat[!duplicated(dat[, c("x", "y", "identifier", "Period")]), ]

}

  dat <- dat[order(dat$year), ]
  
  if (any(is.na(dat$year))) dat <- dat[-which(is.na(dat$year)), ]
  
  for (i in unique(dat$identifier)) {
    
    index <- lapply(unique(dat$Period),
                    function(y) {
                      
                      pDat <- dat[dat$Period == y & dat$identifier == i, ]
                      
                      empDist <- spatstat::nndist(X = pDat$x, Y = pDat$y, k = 1)
                      
                      empMean <- mean(empDist)
                      
                      randomSamp <- lapply(1:nSamps, 
                                           function(x) {
                                             
                                             ran <- raster::sampleRandom(mask, 
                                                                 size = nrow(pDat),
                                                                 xy = T)
                                             
                                             dist <- spatstat::nndist(X = ran[,1], Y = ran[,2], k = 1) 
                                             
                                             NN <- mean(dist)
                                             
                                           })
                      
                      randomSamp <- do.call("c", randomSamp)
                      
                      randomSamp <- randomSamp[is.finite(randomSamp)]
                      
                      indDist <- empMean / randomSamp
                      
                      meanInd <- mean(indDist)
                      
                      upper <- as.numeric(quantile(indDist, prob = 0.95))
                      
                      lower <- as.numeric(quantile(indDist, prob = 0.05))
                      
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
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = Period, y = mean, group = identifier, ymin = lower, ymax = upper, fill = identifier)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(alpha = 0.5) +
    ggplot2::theme_linedraw() + 
    ggplot2::ylab("Nearest neighbour index") +
    ggplot2::labs(fill = "") +
    ggplot2::xlab("")

  return(list(data = data, 
              plot = p))
  
  
}



