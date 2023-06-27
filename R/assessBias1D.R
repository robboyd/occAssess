#' \code{assessBias1D}
#'
#' This function compares the distribution of some variable in the sample to its distribution in the population (i.e. the whole geographic domain). If a set of weights are provided, it also compares the weighted
#' distribution in the sample to the distribution in the population. Multiple sets of weights can be supplied, in which case the function will produce several plots datasets, each pertaining to one set. The idea 
#' is that a sample is representative, at least in terms of the focal variable, if the sample and population distributions are similar. If the weighted sample distribution is closer than the raw sample distribution 
#' to the populaiton distribution, then weighting was successful. Note that the data must be provided in a different format for this function than the others. 
#' 
#' @param pop string. A data.frame with one row for every spatial unit in the population (i.e. the geographic domain) and several columns. At least one column must be a binary variable (1 or 0) taking the value 1 if
#' a spatial unit is in the sample and 0 otherwise. More than one column can contain information on sample inclusion if, say, there are multiple time-periods (see R). 
#' @param R String. Name(s) of the column(s) in pop that indicate whether each unit was included in the sample. If length(R) >1, then several sets of outputs will be provided, one for each R. 
#' @param x String. Name of the column in pop with the variable whose distributions will be assessed. 
#' @param weights Numeric vector or list of numeric vectors. One or more sets of weights whose lengths equal the number of rows in pop. Each weight should should correspond to the matching row of pop. Weights can be calculated using e.g. poststratification,
#' superpopulation modelling, etc. (Boyd et al. 2023). 
#' @param breaks Numeric. \code{assessBias1D} calculates a relative frequency distribution of x in the sample and population, and breaks is the number of bins into which the distributions will be split. 
#' @param RNames String or character vector. Names of sample inclusion variables R. One name per element of R. Used for plots and other outputs.
#' #' @param RNames String or character vector. Names of sample inclusion variables R. One name per element of R. Used for plots and other outputs.
#' @param WNames String or character vector. Name of each set of weights. One name per set (this will often be 1). Used for plots and other outputs.
#' 
#' @export
#' 
#' @return a list with two elements: 1) a plot of the relative frequency distributions and 2) the underlying data. 
#' 
assessBias1D <- function(pop, R, x, weights = NULL, breaks, RNames, WNames = NULL) {
  
  if (!is.data.frame(pop)) stop("pop must be a data.frame")
  
  if(length(weights) != length(WNames)) stop("length(weights) != length(WNames)")
  
  if(length(R) != length(RNames)) stop("length(R) != length(RNames)")

  if (any(!R %in% colnames(pop))) stop("R must be in colnames(pop)")
  
  if (!x %in% colnames(pop)) stop("x must be in colnames(pop)")
  
  dat <- lapply(1:length(R), function(y) {
    
    ind <- ifelse(is.null(weights), 1, length(weights))
    
    stats <- lapply(1:ind, function(z) {
      
      pop$bin <- cut(pop[, x], breaks = breaks, labels = FALSE)
      
      samp <- pop[pop[R[y]] == 1, ]
      
      if (!is.null(weights)) {
        samp$weights <- weights[[z]][[y]]
      }
      
      weightedFreq <- lapply(unique(pop$bin), function(bin_val) {
        bin_samp <- samp[samp$bin == bin_val, ]
        bin_pop <- pop[pop$bin == bin_val, ]
        data.frame(
          weighted_sample = if (!is.null(weights)) {
            sum(bin_samp$weights) / sum(samp$weights)
          } else {
            NA
          },
          sample = nrow(bin_samp) / nrow(samp),
          population = nrow(bin_pop) / nrow(pop),
          bin = bin_val,
          var = z,
          id = RNames[y],
          weightType = ifelse(!is.null(weights), WNames[z], NA)
        )
      })
      
      weightedFreq <- do.call("rbind", weightedFreq)
      reshape2::melt(weightedFreq, id = c("bin", "var", "id", "weightType"))
    })
    
    if (!is.null(weights) | length(R) > 1) {
      stats <- do.call("rbind", stats)
    }
  })
  
  if (!is.null(weights) | length(R) > 1) {
    dat <- do.call("rbind", dat)
  }
  
  if (is.null(weights)) dat <- dat[dat$variable != "weighted_sample", ]
  
  p <- ggplot2::ggplot(data = dat, ggplot2::aes(y = value, x = bin, colour = variable)) +
    ggplot2::geom_line() +
    ggplot2::theme_linedraw() +
    ggplot2::labs(colour = "", x = "", y = "Relative frequency")
  
  if (!is.null(weights) | length(R) > 1) {
    p <- p + ggplot2::facet_grid(id ~ weightType, scales = "free_y")
  }
  
  return(list(plot = p, data = dat))
}

