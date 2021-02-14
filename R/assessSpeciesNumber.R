
assessSpeciesNumber <- function(dat, periods) {
  
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

                                                        data.frame(val = length(unique(dat$species[dat$year == x & !is.na(dat$species)]),
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
  
  ylab <- ifelse(type == "proportion", "Proportion identified to species level", "Number of records identified to species level")
  
  p <- ggplot(data = data, aes(y = val, x = year, fill = Period)) +
    geom_bar(stat = "identity") +
    theme_linedraw() +
    facet_wrap(~group) +
    ylab(ylab)
  
  
  return(list(data = data,
              plot = p))
  
  
}