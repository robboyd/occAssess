#' \code{assessSpatialUncertainty}
#'
#' This function calculates the proportion of records identified to species level in each year with data.
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and spatial uncertainty.
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @return A list with two elements if filter = FALSE and three elements if filter = TRUE. The elements are 1) data (summary of spatial uncertainty),
#'         2) a ggplot object and 3) the input data with the user-defined spatialUncertainty filter applied.
#' @export
#' @examples


assessSpatialUncertainty <- function(dat, periods) {
    
  if (any(!(c("species", "x", "y", "year", "spatialUncertainty", "identifier") %in% colnames(spDat)))) stop("Data must includes columns for species, x, y, year, spatialUncertainty and identifier")
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (any(is.na(dat$year))) {
    
    warning(paste0("Removing", nrow(dat[is.na(dat$year), ]), "records because they are do not have a year associated."))
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }
  
  dat$Period <- NA

  for (i in 1: length(periods)) {

       dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)

  }
  
  props <- lapply(unique(dat$identifier),
                  function(i) {

                    length(dat$species[dat$identifier == i & !is.na(dat$spatialUncertainty)]) /
                      length(dat$species[dat$identifier == i])



                  }


  )

  props <- do.call("rbind", props)

  text <- data.frame(identifier = unique(dat$identifier),
                     prop = round(props, 2),
                     Period = "p1")


p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = spatialUncertainty, group = Period, colour = Period)) +
  ggplot2::geom_histogram() +
  ggplot2::theme_linedraw() +
  ggplot2::facet_wrap(~identifier) +
  ggplot2::xlab("Spatial uncertainty") +
  ggplot2::xlim(0, 15000) +
  ggplot2::geom_text(
    data = text,
    mapping = ggplot2::aes(x = -Inf, y = -Inf, label = paste("Prop. unspecified = ", prop)),
    hjust   = -0.1,
    vjust   = -0.7,
    colour = "black"
  )

  return(list(data = props,
                plot = p))

}


