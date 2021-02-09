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
                          envDat,
                          periods) {

  dat <- dat[-which(is.na(dat$year)), ]
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }

  pca <- rasterPCA(envDat,
                   nComp = 2,
                   spca = T)

  comps <- pca$map

  x <- getValues(comps[[2]])

  x <- x[-which(is.na(x))]

  y <- getValues(comps[[1]])

  y <- y[-which(is.na(y))]

  ## extract PC1 and PC2 scores for the occurrence data

  for (i in unique(dat$Period)) {
    
    assign(paste0(i), dat[dat$Period == i, c("lon", "lat", "identifier")])
    
    assign(paste0(i),data.frame(PC1=  as.numeric(extract(comps[[1]],
                                              get(paste0(i))[, c(1, 2)])),
                     PC2 = as.numeric(extract(comps[[2]],
                                              get(paste0(i))[, c(1, 2)])),
                     Period = as.factor(paste(i)),
                     id = get(paste0(i))[, 3])
    )
    
  }

  data <- lapply(unique(dat$Period),
                 function(x) { get(paste0(x)) })


  data <- do.call("rbind", data)
  
  p <- ggplot(data = data, aes(x=PC2, y=PC1, colour = Period)) +
              geom_point() +
              theme_linedraw() +
              labs(colour = "Period") +
              facet_wrap(~id)


  return(p)

}



