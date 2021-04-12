#' \code{assessSpeciesID}
#'
#' This function calculates the proportion (or counts) of records identified to species level over time..
#' @param dat string. A data.frame containing columns species (species name), x (x coordinate), y (y coordinate), year, spatialUncertainty and identifier. 
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param type String. "proportion" or "count".
#' @param maxSpatUncertainty Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.
#' @return A list with two elements: a ggplot2 object and the data underpinning the plot.
#' @export
#' @examples

assessSpeciesID <- function(dat, periods, type, maxSpatUncertainty = NULL) {

  if (any(!(c("species", "x", "y", "year", "spatialUncertainty", "identifier") %in% colnames(dat)))) stop("Data must includes columns for species, x, y, year, spatialUncertainty and identifier")
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (!is.null(maxSpatUncertainty)) dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]
  
  if (nrow(dat) == 0) stop("No records with with spatialUncertainty < maxSpatUncertainty")
  
  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }
  
  dat$Period <- NA

  for (i in 1: length(periods)) {

    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)

  }

  Ps <- paste0("p", 1:length(periods))
  
  if (any(is.na(dat$year))) {

    warning(paste0("Removing", nrow(dat[is.na(dat$year), ]), "records because they are do not have a year associated."))

    dat <- dat[-which(is.na(dat$year)), ]

  }

  groups <- unique(dat$identifier)

  data <- lapply(groups,
              function(i) {

                dat <- dat[dat$identifier == i, ]

                assign(paste0("props_", i), lapply(Ps,
                                                   function(x) {

                                                     if (type == "proportion") {
                                                       
                                                       prop <- ifelse(length(as.numeric(dat$species[dat$Period == x]) > 0), 1 - (length(dat$species[is.na(dat$species) & dat$Period == x]) /
                                                                                                                       length(dat$species[dat$Period == x])), NA)
                                                       
                                                       data.frame(prop = prop,
                                                                  group = i,
                                                                  Period = x)
                                                       
                                                     } else {
                                                       
                                                       prop <- ifelse(length(as.numeric(dat$species[dat$Period == x]) > 0), length(dat$species[!is.na(dat$species) & dat$Period == x]), 0)
                                                       
                                                       data.frame(prop = prop,
                                                                  group = i,
                                                       
                                                                  Period = x)
                                                       
                                                     }
                                                     })
                )

                assign(paste0("props_", i), do.call("rbind", get(paste0("props_", i))))

              }
  )

  data <- do.call("rbind", data)

  #data <- data[order(data$year), ]
  
  ylab <- ifelse(type == "proportion", "Proportion identified to species level", "Number of records identified to species level")
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(y = prop, x = Period, colour = group, group = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_linedraw() +
    ggplot2::ylab(ylab) +
    ggplot2::labs(colour = "")

  return(list(data = data,
                plot = p))


}

