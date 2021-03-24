#' \code{assessRarityBias}
#'
#' This function assesses the proportionality of species observed range sizes and number of records. For each period, the function regresses
#' the number of grid cells with records (proxy for range size) on the number of records. It uses the r2 value from these regressions
#' as an index proportionality between the two measures. A lower r2 indicates lower proportionality between range size and the number of records relative
#' to a higher score. 
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and year (NA if not known).
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param res Logical. Spatial resolution at which to grid the data for estimation of range size.
#' @export
#' @examples
#' 

assessRarityBias <- function(dat, periods, res) {

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
  
  if (any(is.na(dat$year))) {
    
    warning(paste("Removing", nrow(dat[is.na(dat$year), ]), "records because they are do not have a year associated."))
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  if (any(is.na(dat$species))) {
    
    warning(paste("Removing", nrow(dat[is.na(dat$species), ]), "records because they are do not identified to species level."))
    
    dat <- dat[-which(is.na(dat$species)), ]
    
  }
  
  nSpecies <- lapply(unique(dat$Period),
                     function(x) { x <- length(unique(dat$species[dat$period == x]))})
  
  nSpecies <- do.call("c", nSpecies)
  
  if (any(nSpecies < 30)) warning("There are less than 30 species in some time periods which is a low sample size for the regression. View results with caution.")
  
  
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
    
    x <- lapply(unique(dat$Period[dat$identifier == i]),
                function(y) {
                  
                  spp <- unique(dat$species[dat$Period == y & dat$identifier == i])
                  
                  stats <- lapply(spp, 
                                  function(x) {
                                    r <- raster::rasterize(cbind(dat$x[dat$species == x & dat$identifier == i & dat$Period == y], dat$y[dat$species == x& dat$identifier == i & dat$Period == y]), rast)
                                    cells <- raster::getValues(r)
                                    cells <- length(cells[!is.na(cells)])
                                    recs <- nrow(dat[dat$species == x, ])
                                    
                                    data.frame(species = x,
                                               cells = cells,
                                               recs = recs)
                                    
                                  })
                  
                  stats <- do.call("rbind", stats)
                  
                  mod <- summary(lm(stats$recs ~ stats$cells))$r.squared
                  
                  data.frame(period = y, 
                             id = i,
                             index = mod)
                })
    
    assign(paste0("out", i), do.call("rbind", x))
    
  }
  
  out <- lapply(unique(dat$identifier), 
                function(x) {get(paste0("out", x))})
  
  out <- do.call("rbind", out)
  
  p <- ggplot2::ggplot(data = out, ggplot2::aes(x = period, y = index, colour = id, group = id)) +
    ggplot2::theme_linedraw() +
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::xlab("") +
    ggplot2::ylab("Taxonomic bias index") +
    ggplot2::labs(colour = "")
  
  return(list(plot = p, 
              data = out))
  
}

