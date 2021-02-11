#' \code{assessEnvBias}
#'
#' This function plots the distribution of your data in environmental space. To do this it uses a principal component analysis to reduce your environmental data to two dimensions for visualization.
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and spatial uncertainty.
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param envDat String. A raster::stack object with n layers, one for each environmental variable to be considered. 
#' @return A list with two elements if filter = FALSE and three elements if filter = TRUE. The elements are 1) data (summary of spatial uncertainty),
#'         2) a ggplot object and 3) the input data with the user-defined spatialUncertainty filter applied.
#' @export
#' @examples


assessEnvBias <- function(dat,
                          envCols,
                          periods,
                          ...) {

  if (any(is.na(dat$year))) {
    
    warning("Removing data without a specified year")
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  if (any(is.na(dat[, envCols[1]]))) dat <- dat[-which(is.na(dat[, envCols[1]])), ]
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }

  pca <- prcomp(dat[, envCols])

  p <- autoplot(pca, data = dat, colour = "Period",
                ...) +
    facet_wrap(~identifier) + 
    theme_linedraw()
  
  return(list(pca = pca,
              plot = p))

}

assessEnvBias(dat = dat,
              envCols = 18:35,
              periods = periods,
              ellipse = TRUE,
              scale = TRUE,
              loadings = TRUE,
              loadings.label = TRUE)
