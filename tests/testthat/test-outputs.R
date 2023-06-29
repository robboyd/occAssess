library(occAssess)
library(testthat)

context("test outputs")

data("random40Species")

data("backgroundEnvDat")

data("random40SpeciesEnvDat")

data("callunaData")

periods <- list(2001:2002, 2003:2004, 2005:2006, 2007:2008, 2009:2010)

test_that("check outputs are as expected", {

  expect_equal( assessRecordNumber(dat = random40Species, 
                                   species = "species",
                                   x = "x", 
                                   y = "y",
                                   year = "year",
                                   spatialUncertainty = "spatialUncertainty",
                                   identifier = "identifier",
                                   periods = periods)$data$val[1], 3968 )
  
  expect_equal( assessRarityBias(dat = random40Species, 
                                 species = "species",
                                 x = "x", 
                                 y = "y",
                                 year = "year",
                                 spatialUncertainty = "spatialUncertainty",
                                 identifier = "identifier",
                                 periods = periods, 
                                 res = 10000,
                                 prevPerPeriod = TRUE)$data$index[1], 0.9992043 )
  
  expect_equal( assessSpeciesNumber(dat = random40Species, 
                                    species = "species",
                                    x = "x", 
                                    y = "y",
                                    year = "year",
                                    spatialUncertainty = "spatialUncertainty",
                                    identifier = "identifier",
                                    periods = periods)$data$val[1], 40 )
  
  expect_equal( assessSpeciesID(dat = random40Species, 
                                species = "species",
                                x = "x", 
                                y = "y",
                                year = "year",
                                spatialUncertainty = "spatialUncertainty",
                                identifier = "identifier",
                                periods = periods, type = "proportion")$data$prop[1], 1 )
  
  expect_equal( length(assessSpatialCov(dat = random40Species, 
                                        species = "species",
                                        x = "x", 
                                        y = "y",
                                        year = "year",
                                        spatialUncertainty = "spatialUncertainty",
                                        identifier = "identifier",
                                        countries = "UK",
                                        res = 20000, periods = periods, output = "overlap")), 2)
  
  expect_equal( length(assessSpatialCov(dat = random40Species, 
                                        species = "species",
                                        x = "x", 
                                        y = "y",
                                        year = "year",
                                        spatialUncertainty = "spatialUncertainty",
                                        identifier = "identifier",
                                        countries = "UK",
                                        res = 20000, 
                                        periods = periods, 
                                        output = "overlap",
                                        returnRaster = TRUE)), 2)
  
  expect_true( is.list(assessSpatialCov(dat = random40Species, 
                                        species = "species",
                                        x = "x", 
                                        y = "y",
                                        year = "year",
                                        spatialUncertainty = "spatialUncertainty",
                                        identifier = "identifier",
                                        res = 20000, periods = periods, countries = "UK")))
  
  expect_true( length(assessSpatialCov(dat = random40Species, 
                                       species = "species",
                                       x = "x", 
                                       y = "y",
                                       year = "year",
                                       spatialUncertainty = "spatialUncertainty",
                                       identifier = "identifier",
                                       res = 20000, periods = periods, countries = "UK")) == 2)
  
  expect_true( length(assessSpatialCov(dat = random40Species,
                                       species = "species",
                                       x = "x", 
                                       y = "y",
                                       year = "year",
                                       spatialUncertainty = "spatialUncertainty",
                                       identifier = "identifier",
                                       res = 20000, 
                                       periods = periods,
                                       output = "nPeriods",
                                       countries = "UK")) == 2)

  expect_equal( assessEnvBias(dat = random40Species,
                              species = "species",
                              x = "x", 
                              y = "y",
                              year = "year",
                              spatialUncertainty = "spatialUncertainty",
                              identifier = "identifier",
                              periods = periods,
                              envDat = random40SpeciesEnvDat,
                              backgroundEnvDat = backgroundEnvDat,
                              xPC = 1,
                              yPC = 2)[[1]]$scores.PC1[1], -159.4986, tolerance = 1)
  
  expect_true( length(assessBias1D(pop = callunaData,
                                   breaks = 50, 
                                   R = c("sampled_units_1987.1999", "sampled_units_2010.2019"),
                                   x = "road_length_299_neighbours",
                                   RNames = c("Period_1", "Period_2"))) == 2)
  
})


