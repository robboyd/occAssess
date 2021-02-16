#' \code{assessSpatialCov}
#'
#' This function grids occurrence data then maps it as counts per grid cell in geographic spece. .
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and year (NA if not known).
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param res Numeric. Spatial resolution at which to grid the occurrence data.
#' @param logCount Logical. Whether to log transform counts for visual purposes. Useful where there is large variation in counts across cells. 
#' @seealso \code{\link{assessSpeciesID}} which gives the number of species identified to species level. 
#' @export
#' @examples

assessSpatialCov <- function (dat, periods, res, logCount = FALSE, countries) {
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }

  xmin <- min(dat$lon, na.rm = T)
  
  xmax <- max(dat$lon, na.rm = T)
  
  ymin <- min(dat$lat, na.rm = T)
  
  ymax <- max(dat$lat, na.rm = T)
  
  rast <- raster::raster(ncol=length(seq(xmin, xmax, res)),
                         nrow=length(seq(ymin, ymax, res)),
                         xmn=xmin,
                         xmx=xmax,
                         ymn=ymin,
                         ymx=ymax)
  

  rasts <- lapply(X=unique(dat$identifier), 
                  function(x) { data <- dat[dat$identifier == x, c("lon", "lat")]
                                raster::rasterize(data, rast)})
  
  rasts <- raster::stack(rasts)
  
  if (logCount == TRUE) {
    
    leg <- "log10(n records)"
    
    rasts <- log10(rasts)
    
  } else {
    
    leg <- "n records"
    
  }
  
  names(rasts) <- unique(dat$identifier)
  
  map <- ggplot2::map_data("world", regions = countries)
  
  myCol <- rgb(255,255,255, max = 255, alpha = 125, names = "blue50")

  rasterVis::gplot(rasts) + ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::geom_polygon(data = map, ggplot2::aes(x=long, y = lat, group = group),
                 colour="black",fill=myCol,
                 inherit.aes=F) +
    ggplot2::facet_wrap(~ variable) + 
    ggplot2::theme_linedraw() +
    ggplot2::scale_fill_gradient2(low = "red", high = "blue", na.value = myCol,
                                  name = leg) 

  
}

