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
                   
                   assign(paste0("props_", i), lapply(unique(dat$year),
                                                      function(x) {
                                                        
                                                        p <- dat$Period[dat$year == x][1]
                                                        
                                                        data.frame(val = length(dat$species[dat$year == x]),
                                                                   year = x,
                                                                   group = i,
                                                                   Period = p)
                                                      })
                   )
                   
                   assign(paste0("props_", i), do.call("rbind", get(paste0("props_", i))))
                   
                 }
  )
  
  data <- do.call("rbind", data)
  
  data <- data[order(data$year), ]
  
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(y = val, x = year, fill = Period)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_linedraw() +
    ggplot2::facet_wrap(~group) +
    ggplot2::ylab("Number of records")
  
  
  return(list(data = data,
              plot = p))
  
  
}


