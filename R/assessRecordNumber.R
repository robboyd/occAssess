#' \code{assessRecordNumber}
#'
#' This function calculates the number of records in each year.
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and year (NA if not known).
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @seealso \code{\link{assessSpeciesID}} which gives the number of species identified to species level in each year. 
#' @export
#' @examples

assessRecordNumber <- function(dat, periods) {
  
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
    
    warning(paste0("Removing", nrow(dat[is.na(dat$year), ]), "records because they are do not have a year associated."))
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  groups <- unique(dat$identifier)
  
  data <- lapply(groups,
                 function(i) {
                   
                   dat <- dat[dat$identifier == i, ]
                   
                   assign(paste0("props_", i), lapply(unique(dat$Period),
                                                      function(x) {
                                                        
                                                        data.frame(val = length(dat$species[dat$Period == x]),
                                                                   group = i,
                                                                   Period = x)
                                                      })
                   )
                   
                   assign(paste0("props_", i), do.call("rbind", get(paste0("props_", i))))
                   
                 }
  )
  
  data <- do.call("rbind", data)
  
  #data <- data[order(data$year), ]
  
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(y = val, x = Period, colour = group, group = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_linedraw() +
    ggplot2::ylab("Number of records") +
    ggplot2::labs(colour = "")

  return(list(data = data,
              plot = p))
  
  
}


