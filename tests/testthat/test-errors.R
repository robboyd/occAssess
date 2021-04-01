library(occAssess)
library(testthat)

context("errors and warnings")

data(random)

periods <- list(2000:2004, 2005:2010)

test_that("check basic errors are caught", {
  
  expect_error( assessSpatialUncertainty(dat = random[, -1], periods = periods) )
  
  random$identifier[10] <- NA
  
  expect_error( assessSpatialUncertainty(dat = random, periods = periods))
  
})


