#' \code{assessSpatialBias}
#'
#' This function calculates the proportion of records identified to species level in each year with data.
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and spatial uncertainty.
#' @param periods Numeric. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param nSamps Logical. How many iterations of random samples to use for comparison of empirical NN index with random NN index.
#' @return A list with two elements if filter = FALSE and three elements if filter = TRUE. The elements are 1) data (summary of spatial uncertainty),
#'         2) a ggplot object and 3) the input data with the user-defined spatialUncertainty filter applied.
#' @export
#' @examples

assessSpatialBias <- function(dat, periods, nSamps = 100) {
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }
  
  if (any(is.na(dat$year))) dat <- dat[-which(is.na(dat$year)), ]
  
  for (i in unique(dat$identifier)) {
    
    index <- lapply(unique(dat$Period),
                    function(y) {
                      
                      pDat <- dat[dat$Period == y & dat$identifier == i, ]
                      
                      empDist <- nndist(X = pDat$lon, Y = pDat$lat, k = 1)
                      
                      empMean <- mean(empDist)
                      
                      randomSamp <- lapply(1:nSamps, 
                                           function(x) {
                                             
                                             ran <- sampleRandom(rast, 
                                                                 size = nrow(pDat),
                                                                 xy = T)
                                             
                                             dist <- nndist(X = ran[,1], Y = ran[,2], k = 1) 
                                             
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
  
  p <- ggplot(data = data, aes(x = Period, y = mean, group = identifier, ymin = lower, ymax = upper, fill = identifier)) +
    geom_line() +
    geom_point() +
    geom_ribbon(alpha = 0.5) +
    theme_linedraw() + 
    ylab("Nearest neighbour index") +
    labs(colour = "")
  
  return(list(data = data, 
              plot = p))
  
  
}
