library(occAssess)
library(testthat)

context("errors")

data(random)

dat <- random[, -1]

periods <- list(2000:2004, 2005:2010)

test_that("check functions error with wrong column names", {
  
  expect_error( assessSpatialUncertainty(dat = dat, periods = periods) )
  
})

