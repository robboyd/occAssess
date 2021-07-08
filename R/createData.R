#### internal function to convert users' data to occAssess-friendly format

createData <- function(data, 
                       species,
                       x,
                       y,
                       year,
                       spatialUncertainty,
                       identifier) {
  
  dat <- data.frame(species = data[, species],
                    x = data[, x],
                    y = data[, y],
                    year = data[, year],
                    spatialUncertainty = data[, spatialUncertainty],
                    identifier = data[, identifier])
  
}

