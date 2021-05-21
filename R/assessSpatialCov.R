#' \code{assessSpatialCov}
#'
#' This function grids and then maps species occurrence data.
#' @param dat string. A data.frame containing columns species (species name), x (x coordinate), y (y coordinate), year, spatialUncertainty and identifier. 
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param res Numeric. Spatial resolution at which to grid the occurrence data.
#' @param logCount Logical. Whether to log transform counts for visual purposes. Useful where there is large variation in counts across cells. 
#' @param countries String or vector. Country names to be passed to ggplot2::map_data. Needed to add country borders to your plot. Countries should only be used if you are working on
#'        the WGS84 coordinate reference system. Otherwise see shp. If the levels in the identifier field of dat are spatial units (e.g. countries), then the countries argument should be provided a list object. 
#'        The names of each element in the list should correspond to one level of identifier, and the contents of that element should be a country name that is valid for use with ggplot2::map_data. Both countries and shp may 
#'        be left unspecified, but in this case no country borders will be presented on the returned plots. Do not specify both countries and shp.
#' @param shp String. If you are not working on WGS84, then you can provide a shapefile (spatialPolygons or spatialPolygonsDataFrame) with the country borders on the relevant crs for plotting. As with countries, 
#'            if the levels in identifier denote spatial subsets of the data, then shp may be provided as a list, with the name of each element corresponding to one level of the identifier field in dat. 
#'            Do not specify both countries and dat.
#' @param maxSpatUncertainty Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.
#' @param output String. Either "density", "overlap" or "nPeriods". If density then the maps show the density of records (grid cell^-1) per period and level of identifier. 
#'               If overlap then one map is returned per level of identifier showing which cells have been sampled in >= \code{minPeriods} periods.
#'               If nPeriods then one map is returned per level of identifier showing the number of periods in which each grid cell has been sampled.
#' @param minPeriods Numeric. Lower limit of periods with sampling for cells to show up on the map. Defaults to NULL in which case only grid cells sampled in all \code{periods} are shown. This argument
#'                   only applies if \code{output} = "overlap".
#' @return By default alist with n ggplot2 objects where n is the number of levels in the identifier field of dat. Where \code{output} = density return one map per level of identifier.
#' @seealso \code{\link{assessSpatialBias}} which gives a measure of how far your data eviates from a random distribution in space. 
#' @importFrom rasterVis gplot
#' @export

assessSpatialCov <- function(dat, 
                             res, 
                             logCount = FALSE, 
                             countries = NULL, 
                             shp = NULL, 
                             periods, 
                             maxSpatUncertainty = NULL,
                             output = "density",
                             minPeriods = NULL) {

  if (!output %in% c("density", "overlap", "nPeriods")) stop("Output must be one of density, overlap or nPeriods")
  
  if (is.list(countries) | is.list(shp)) print("You have specified shp or countries as a list object; this should only be the case if your identifier field denotes spatial subsets of your data.")
  
  if (!is.null(shp) & !is.null(countries)) stop("Only one of countries and shp may be specified; they do the same thing. If you are working on WGS84 it is recommended that you use countries; otherwise, you cannot use countries and should use shp.")
  
  if (any(!(c("species", "x", "y", "year", "spatialUncertainty", "identifier") %in% colnames(dat)))) stop("Data must includes columns for species, x, y, year, spatialUncertainty and identifier")
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (!is.null(maxSpatUncertainty)) dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]

  if (nrow(dat) == 0) stop("No records with with spatialUncertainty < maxSpatUncertainty")
  
  if (any(is.na(dat$year))) {
    
    warning("Removing data without a specified year")
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
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
                    
                    if (nrow(data) >= 1) {
                      
                      raster::rasterize(data, rast)
                      
                    } else {
                      
                      r <- raster::setValues(rast, NA)
                      
                      r
                      
                    }
                    
                    })
                    

    names(rasts) <- unique(dat$Period)

    rasts <- raster::stack(rasts)

    if (logCount == TRUE) rasts <- log10(rasts)

    if (output == "density") {
      
      assign(paste0("rasts", i), rasts)
      
    } else if (output == "nPeriods") {
      
      rasts <- sum(as.logical(rasts), na.rm = T)
      
      rasts[rasts == 0] <- NA
      
      assign(paste0("rasts", i), rasts)
      
    } else {

      if (is.null(minPeriods)) minPeriods <- length(unique(dat$Period))

      rasts <- sum(as.logical(rasts), na.rm = T)

      rasts[rasts < minPeriods] <- NA
      
      assign(paste0("rasts", i), as.logical(rasts))
      
    }
    
  }

  if (output == "overlap") {
    
    leg <- "sampled in minPeriods
    periods"
    
  } else if (output == "density") {
    
    leg <- ifelse(logCount == TRUE, "log10(n records)", "n records")
    
  } else {
    
    leg <- "Number of periods sampled"
    
  }
  
  
  myCol <- rgb(255,255,255, max = 255, alpha = 125, names = "blue50")

  ## add lapply to return multiple plots as a list

  out <- lapply(as.character(unique(dat$identifier)),
                function(x) {
                  
                  if (is.list(countries) & !any(!countries %in% unique(ggplot2::map_data("world")$region)) |
                      !is.list(countries) & !is.null(countries) & !any(!countries %in% unique(ggplot2::map_data("world")$region)) |
                      !is.null(shp)) {
                    
                    if (is.null(shp)) { 
                      
                      if (is.list(countries)) {
                        
                        map <- ggplot2::map_data("world", regions = countries[[x]])
                        
                      } else {
                        
                        map <- ggplot2::map_data("world", regions = countries)
                        
                      }
                      
                    } else { 
                      
                      if (is.list(shp)) {
                        
                        map <- ggplot2::fortify(shp[[x]])
                        
                      } else {
                        
                        map <- ggplot2::fortify(shp)
                        
                      }
                      
                    }
                    
                  } else {
                    
                    warning("Some or all country names provided are not in unique(ggplot2::map_data(world)$region)")
                    
                    map <- NULL
                    
                  }


                  p <- rasterVis::gplot(get(paste0("rasts", x))) +
                    ggplot2::theme_linedraw()
                  
                  if (output == "density") {
                    
                    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = value)) +
                      ggplot2::facet_wrap(~ variable) + 
                      ggplot2::scale_fill_gradient2(low = "red", high = "blue", na.value = myCol,
                                                    name = leg)
                    
                  } else {
                    
                    n <- length(unique(getValues(get(paste0("rasts", x)))))
                    
                    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = factor(value))) +
                      ggplot2::scale_fill_manual(values = terrain.colors(n, rev = TRUE), 
                                                 na.value = myCol, name = leg,
                                                 na.translate = FALSE) +
                      ggplot2::ggtitle(x)
                  
                    if (output == "overlap") p <- p + ggplot2::theme(legend.position = "none") 
                    
                  }

                  if (!is.null(map)) p <- p + ggplot2::geom_polygon(data = map, ggplot2::aes(x=long, y = lat, group = group),
                                                                                 colour="black",fill=myCol,
                                                                                 inherit.aes=F)
                  return(p)
                  
                  })
  
  names(out) <- unique(dat$identifier)

return(out)

}

