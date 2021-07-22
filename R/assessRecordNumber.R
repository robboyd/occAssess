#' \code{assessRecordNumber}
#'
#' This function calculates the number of records in each year.
#' @param dat string. A data.frame containing columns for species name, x coordinates, y coordinates, spatialUncertainty, year and an identifier (used to group the data - heuristic will be calculated for each group). 
#' @param species Character string. column name in dat giving species names.
#' @param x string. Column name in dat giving x coordinates. Any coordinate and spatial reference systems are permitted.
#' @param y string. Column name in dat giving y coordinates. Any coordinate and spatial reference systems are permitted.
#' @param year string. Column name in dat giving years.
#' @param spatialUncertainty String. Column name in dat giving uncertainty associated with x and y. Any units are permitted. 
#' @param identifier String. Column name in dat giving record "identifiers". Identifiers are used to group the data; heuristics will be calculated separately for each group.
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param maxSpatUncertainty Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.
#' @param normalize Logical. Whether or not to normalize the counts for each level of identifier. This is helpful where the number of records varies widely among levels of identifier.
#' @return A list with two elements: a ggplot2 object and the data underpinning the plot.
#' @seealso \code{\link{assessSpeciesNumber}} which gives the number of species recorded each period. 
#' @export

assessRecordNumber <- function(dat, 
                               species,
                               x,
                               y,
                               year,
                               spatialUncertainty,
                               identifier,
                               periods, 
                               maxSpatUncertainty = NULL,
                               normalize = FALSE) {
  
  if (any(!(c(species, x, y, year, spatialUncertainty, identifier) %in% colnames(dat)))) stop("You have specified columns that don't exist in dat.")
  
  dat <- createData(data = dat,
                    species,
                    x,
                    y,
                    year,
                    spatialUncertainty,
                    identifier)
  
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
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], i, dat$Period)
    
  }
  
  if (any(is.na(dat$year))) {
    
    warning(paste0("Removing", nrow(dat[is.na(dat$year), ]), "records because they are do not have a year associated."))
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  groups <- unique(dat$identifier)
  
  data <- lapply(groups,
                 function(i) {
                   
                   dat <- dat[dat$identifier == i, ]
                   
                   assign(paste0("props_", i), lapply(1:length(periods),
                                                      function(x) {
                                                        
                                                        val <- ifelse(length(dat$species[dat$Period == x]) > 0, length(dat$species[dat$Period == x]), 0)
                                                        
                                                        data.frame(val = val,
                                                                   group = i,
                                                                   Period = x)
                                                      })
                   )
                   
                   assign(paste0("props_", i), do.call("rbind", get(paste0("props_", i))))
                   
                 }
  )
  
  data <- do.call("rbind", data)
  
  if (normalize) {
    
    for (i in groups) {
      
      data$val[data$group == i] <- data$val[data$group == i] / max(data$val[data$group == i])
      
    }
    
  }
  
  #data <- data[order(data$year), ]
  
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(y = val, x = Period, colour = group, group = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_linedraw() +
    ggplot2::ylab("Number of records") +
    ggplot2::labs(colour = "",
                  x = "Period")

  return(list(data = data,
              plot = p))
  
  
}


