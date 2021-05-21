library(occAssess)
library(testthat)

context("errors and warnings")

test_that("check basic errors are caught", {
  
  data("random40Species")
  
  data("UKWGS84mask")
  
  mask <- raster::stack(UKWGS84mask, UKWGS84mask)
  
  periods <- list(2000:2004, 2005:2010)
  
  expect_error( assessRecordNumber(dat = random40Species[, -1], periods = periods) )
  
  expect_error( assessRecordNumber(dat = random40Species[dat$species == "human", ], periods = periods) )
  
  expect_error( assessSpatialBias(dat = random40Species, periods = periods, mask = mask, nSamps = 3) )
  
  expect_error( assessSpatialCov(dat = random40Species, periods = periods, res = 20000, 
                                 countries = "UK", shp = "shp", logCount = TRUE))
  
  expect_error( assessSpatialCov(dat = random40Species, periods = periods, res = 20000, 
                                 countries = "UK", logCount = TRUE, output = "hello"))
  
  random40Species$identifier[10] <- NA
  
  expect_error( assessRecordNumber(dat = random40Species, periods = periods))
  
  
})


