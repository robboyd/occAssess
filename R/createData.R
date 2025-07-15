#' @import dplyr
#### internal function to convert users' data to occAssess-friendly format
createData <- function(data, 
                       species,
                       x,
                       y,
                       year,
                       spatialUncertainty,
                       identifier) {

  dat <- data.frame(species = data[, species],
                    x = pull(data, x),
                    y = pull(data, y),
                    year = pull(data, year),
                    spatialUncertainty = pull(data, spatialUncertainty),
                    identifier = pull(data, identifier))

    return(dat)
}

