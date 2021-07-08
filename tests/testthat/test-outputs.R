library(occAssess)
library(testthat)

context("test outputs")

data("random40Species")

data("backgroundEnvDat")

data("random40SpeciesEnvDat")

periods <- list(2001:2002, 2003:2004, 2005:2006, 2007:2008, 2009:2010)

test_that("check outputs are as expected", {
  
  expect_equal( assessRecordNumber(dat = random40Species, periods = periods)$data$val[1], 3968 )
  
  expect_equal( assessRarityBias(dat = random40Species, 
                                 periods = periods, 
                                 res = 10000,
                                 prevPerPeriod = TRUE)$data$index[1], 0.9992043 )
  
  expect_equal( assessSpeciesNumber(dat = random40Species, periods = periods)$data$val[1], 40 )
  
  expect_equal( assessSpeciesID(dat = random40Species, periods = periods, type = "proportion")$data$prop[1], 1 )
  
  expect_equal( length(assessSpatialCov(dat = random40Species, res = 20000, periods = periods, output = "overlap")), 2)
  
  expect_true( is.list(assessSpatialCov(dat = random40Species, res = 20000, periods = periods, countries = "UK")))
  
  expect_true( length(assessSpatialCov(dat = random40Species, res = 20000, periods = periods, countries = "UK")) == 2)
  
  expect_true( length(assessSpatialCov(dat = random40Species, 
                                       res = 20000, 
                                       periods = periods,
                                       output = "nPeriods",
                                       countries = "UK")) == 2)
  
  expect_equal( assessEnvBias(dat = random40Species,
                periods = periods,
                envDat = random40SpeciesEnvDat,
                backgroundEnvDat = backgroundEnvDat,
                xPC = 1,
                yPC = 2)[[1]]$scores.PC1[1], 577.8804)
  
})



