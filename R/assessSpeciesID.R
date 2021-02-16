#' \code{assessSpeciesID}
#'
#' This function calculates the proportion (or counts) of records identified to species level over time..
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and year (NA if not known).
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param filter Logical. Whether or not to return the input data after filtering based on a threshold spatialUncertainty.
#' @param threshold Numeric. Threshold to be used if filter = TRUE.
#' @return A list with two elements if filter = FALSE and three elements if filter = TRUE. The elements are 1) data (summary of proportions identified over time),
#'         2) a ggplot object and 3) the input data with the user-defined spatialUncertainty filter applied.
#' @export
#' @examples


assessSpeciesID <- function(dat, periods, type) {

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

                                                     if (type == "proportion") {
                                                       
                                                       data.frame(prop = 1 - (length(dat$species[is.na(dat$species) & dat$year == x]) /
                                                                                length(dat$species[dat$year == x])),
                                                                  year = x,
                                                                  group = i,
                                                                  Period = p)
                                                       
                                                     } else {
                                                       
                                                       data.frame(prop = length(dat$species[!is.na(dat$species) & dat$year == x]),
                                                                  year = x,
                                                                  group = i,
                                                                  Period = p)
                                                       
                                                     }
                                                     })
                )

                assign(paste0("props_", i), do.call("rbind", get(paste0("props_", i))))

              }
  )

  data <- do.call("rbind", data)

  data <- data[order(data$year), ]
  
  ylab <- ifelse(type == "proportion", "Proportion identified to species level", "Number of records identified to species level")
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(y = prop, x = year, fill = Period)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_linedraw() +
    ggplot2::facet_wrap(~group) +
    ggplot2::ylab(ylab)


  return(list(data = data,
                plot = p))


}

