#' \code{assessSpatialCov}
#'
#' This function grids occurrence data then maps it as counts per grid cell in geographic spece. .
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and year (NA if not known).
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param res Numeric. Spatial resolution at which to grid the occurrence data.
#' @param logCount Logical. Whether to log transform counts for visual purposes. Useful where there is large variation in counts across cells. 
#' @param countries. String or vector. Country names to be passed to ggplot2::map_data. Needed to add borders to your plot. Countries should only be used if you are working on
#'        the WGS84 coordinate reference system. Otherwise see shp. 
#' @param shp String. If you are not working on WGS84, then you will need to provide a shapefile (spatialPolygons or spatialPolygonsDataFrame) with the country borders on the relevant crs for plotting. 
 
#' @export
#' @examples

assessSpatialCov <- function (dat, res, logCount = FALSE, countries, shp = NULL, periods) {

  if (any(!(c("species", "x", "y", "year", "spatialUncertainty", "identifier") %in% colnames(dat)))) stop("Data must includes columns for species, x, y, year, spatialUncertainty and identifier")
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (any(is.na(dat$year))) {
    
    warning("Removing data without a specified year")
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  dat <- dat[order(dat$year), ]
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)
    
  }
  
  xmin <- min(dat$x, na.rm = T)
  
  xmax <- max(dat$x, na.rm = T)
  
  ymin <- min(dat$y, na.rm = T)
  
  ymax <- max(dat$y, na.rm = T)
  
  rast <- raster::raster(ncol=length(seq(xmin, xmax, res)),
                         nrow=length(seq(ymin, ymax, res)),
                         xmn=xmin,
                         xmx=xmax,
                         ymn=ymin,
                         ymx=ymax)
  
  for (i in unique(dat$identifier)) {
    
    rasts <- lapply(X=unique(dat$Period), 
                    function(x) { data <- dat[dat$identifier == i & dat$Period == x, c("x", "y")]
                    raster::rasterize(data, rast)})
    
    names(rasts) <- unique(dat$Period)
    
    assign(paste0("rasts", i), raster::stack(rasts))
    
  }
  
  if (logCount == TRUE) {
    
    leg <- "log10(n records)"

    lapply(1:length(unique(dat$identifier)),
           function(x) {assign(paste0("rasts", unique(dat$identifier[x])), log10(get(paste0("rasts", unique(dat$identifier[x])))))})
    
  } else {
    
    leg <- "n records"
    
  }

  #map <- ifelse(is.null(shp), ggplot2::map_data("world", regions = countries), ggplot2::fortify(shp)) 

  if (is.null(shp)) { map <- ggplot2::map_data("world", regions = countries) }
  else { map <- ggplot2::fortify(shp)}
  
  myCol <- rgb(255,255,255, max = 255, alpha = 125, names = "blue50")

  ## add lapply to return multiple plots as a list
  
  out <- lapply(unique(dat$identifier),
                function(x) {rasterVis::gplot(get(paste0("rasts", x))) + 
                    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
                    #ggplot2::geom_polygon(data = map, ggplot2::aes(x=long, y = lat, group = group),
                    #             colour="black",fill=myCol,
                    #             inherit.aes=F) +
                    ggplot2::geom_path(data = map, 
                                       ggplot2::aes(x = long, y = lat, group = group),
                                       color = "black") +
                    ggplot2::facet_wrap(~ variable) + 
                    ggplot2::theme_linedraw() +
                    ggplot2::scale_fill_gradient2(low = "red", high = "blue", na.value = myCol,
                                                  name = leg)})
  
  names(out) <- unique(dat$identifier)

return(out)
  
}

