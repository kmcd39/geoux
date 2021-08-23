library(tidyverse)
library(geox)
library(sf)



testthat::test_that("spatializing tracts", {
  cts <- xwalks::ctx[1:10, 'geoid']
  ctsf <- attach.geos(cts)
  testthat::expect_s3_class(ctsf, "sf")
})



#build.CZs(.czs = c('19700')) %>% mapview::mapview()
#czs <- build.CZs()

#czs[1:10,2] %>% mapview::mapview()
