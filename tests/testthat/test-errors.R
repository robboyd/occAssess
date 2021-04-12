library(occAssess)
library(testthat)

context("errors and warnings")

data("random40Species")

periods <- list(2000:2004, 2005:2010)

test_that("check basic errors are caught", {
  
  expect_error( assessRecordNumber(dat = random40Species[, -1], periods = periods) )
  
  expect_error( assessRecordNumber(dat = random40Species[dat$species == "human", ], periods = periods) )
  
  random40Species$identifier[10] <- NA
  
  expect_error( assessRecordNumber(dat = random40Species, periods = periods))
  
})


