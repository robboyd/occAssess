#' \code{assessRepeatVisits}
#'
#' This function calculates the number of repeat visits across cells of a user-defined spatial resolution. Repeats are divided
#' by the number of years in each user-defined period to give the average number of repeat samples per year on each grid cell.
#'
#' @param dat String. Data.frame with columns for lat, lon, and identifier (usually taxonomic group name).
#' @param periods String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param spatRes Numeric. Spatial resolution at which to calculate repeat visits. Depends on CRS (use metres for eastings and northings, and decimals for lat lon).
#' @export
#' @examples

assessRepeatVisits <- function(dat, periods, spatRes) {

  dat$Period <- NA

  for (i in 1: length(periods)) {

    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", i), dat$Period)

    assign(paste0("p", i, "L"), length(periods[[i]]))

  }

  rast <- raster::raster(ncol=length(seq(-120,-30,spatRes)),
                         nrow=length(seq(-60,30,spatRes)),
                         xmn=-120,
                         xmx=-30,
                         ymn=-60,
                         ymx=30)

  out <- lapply(unique(dat$identifier),
                function(i) {

                  for (p in 1:length(periods)) {

                    assign(paste0("dat", p), dat[dat$Period == paste0("p", p) & dat$identifier == i, ])

                    assign(paste0("data", p), data.frame(get(paste0("dat", p))[, c("lon", "lat")]))

                    assign(paste0("rDat", p), raster::rasterize(x = get(paste0("data", p)), y = rast, fun = "count"))

                    assign(paste0("out", p), raster::getValues(get(paste0("rDat", p))))

                    assign(paste0("out", p), get(paste0("out", p)) / get(paste0("p", p, "L")))

                  }

                  vals <- lapply(1:length(periods),
                                 function(x) { data.frame(vals = get(paste0("out", x)),
                                                          Period = paste0("p", x))}
                  )

                  vals <- do.call("rbind", vals)

                  out <- data.frame(vals = vals$vals,
                                    identifier = i,
                                    Period = vals$Period)

         })

    out <- do.call("rbind", out)

    p <- ggplot2::ggplot(data=out, ggplot2::aes(x = vals, fill = Period)) +
             ggplot2::geom_histogram(colour = "black") +
      ggplot2::xlab("Visits / years") +
      ggplot2::theme_linedraw() +
      xlim(c(0, 20)) +
      facet_wrap(~identifier)

    return(list(data = out,
                plot = p))

}


