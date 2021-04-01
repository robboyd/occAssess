library(occAssess)
library(ggfortify)
library(ggplot2)

context("test outputs")

data(random)

periods <- list(2000:2004, 2005:2010)

test_that("check outputs are as expected", {
  
  expect_equal( assessSpatialUncertainty(dat = random, periods = periods)$data[1,1], 1 )
  
  expect_equal( assessRarityBias(dat = random, periods = periods, res = 10000)$data$index[1], 0.05389407 )
  
  expect_equal( assessSpeciesNumber(dat = random, periods = periods)$data$val[1], 5 )
  
  expect_equal( assessSpeciesID(dat = random, periods = periods, type = "proportion")$data$prop[1], 1 )
  
  expect_equal( length(assessEnvBias(dat = random, periods = periods, nEnvVar = 19)$pca[[1]]), 19 )
  
})



