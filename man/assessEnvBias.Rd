% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assessEnvBias.R
\name{assessEnvBias}
\alias{assessEnvBias}
\title{\code{assessEnvBias}}
\usage{
assessEnvBias(
  dat,
  periods,
  envDat,
  backgroundEnvDat = NULL,
  xPC = 1,
  yPC = 2,
  maxSpatUncertainty = NULL
)
}
\arguments{
\item{dat}{string. A data.frame containing columns for species name (species), x and y coordinates (x, y), spatialUncertainty, year and uncertainty.}

\item{periods}{String. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).}

\item{envDat}{String. A dataframe with n columns and N rows where n are environmental variables and N are the values of those variables at the locations of x and y in the occurrence data. N must be the same length as the coordinates in dat.}

\item{backgroundEnvDat}{String. As envDat but N do not correspond to the locations of the occurrence data; instead, they are a background sample of environmental space in the study region. N does not have to be the same length as the coordinates in dat. backgroundEnvDat defaults to NULL in which case the function is calculated only for the sampled environmental space.}

\item{xPC}{Numeric. Which principal component to use as the x axis.}

\item{yPC}{Numeric. As xPC but for the y axis.}

\item{maxSpatUncertainty}{Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.}
}
\value{
a ggplot2 object with separate panels for each level of identifier in dat.
}
\description{
This function plots the distribution of your data in environmental space. To do this it uses a principal component analysis to reduce your environmental data to two dimensions for visualization.
}
