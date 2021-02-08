#' \code{assessSpatialUncertainty}
#'
#' This function calculates the proportion of records identified to species level in each year with data.
#' @param dat string. A data.frame containing columns for species name (NA if not identified), an identifier (usually taxonomic group name),
#'            and spatial uncertainty.
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param filter Logical. Whether or not to return the input data after filtering based on a threshold spatialUncertainty.
#' @param threshold Numeric. Threshold to be used if filter = TRUE.
#' @return A list with two elements if filter = FALSE and three elements if filter = TRUE. The elements are 1) data (summary of spatial uncertainty),
#'         2) a ggplot object and 3) the input data with the user-defined spatialUncertainty filter applied.
#' @export
#' @examples


assessSpatialUncertainty <- function(dat, periods, filter = F, threshold = NULL) {

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


p <- ggplot(data = dat, aes(x = spatialUncertainty, fill = Period)) +
  geom_histogram() +
  theme_linedraw() +
  facet_wrap(~identifier) +
  xlab("Spatial uncertainty (m)") +
  xlim(0, 15000) +
  geom_text(
    data = text,
    mapping = aes(x = -Inf, y = -Inf, label = paste("Prop. unspecified = ", prop), fill = Period),
    hjust   = -0.1,
    vjust   = -0.7
  )

  if (filter == TRUE) {

    dat <- dat[-which(dat$spatialUncertainty > threshold), ]

    out <- list(data = props,
                plot = p,
                filterDat = dat)

  } else {

    out <- list(data = props,
                plot = p)

  }

return(out)

}



