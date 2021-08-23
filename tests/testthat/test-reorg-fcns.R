library(tidyverse)
library(geox)

phl <- tibble(cz = '19700')



testthat::test_that("region rerog", {
  testthat::expect_equal(
    tibble(rt = 'cz',
           rid = '19700'),
      region.reorg(phl, 'cz'
                 ,abvcols = T)
  )
})




testthat::test_that("region names", {
  testthat::expect_equal(
    tibble(rt = 'cz',
           rid = '19700',
           rn = 'Philadelphia'),
    add.rns(
      region.reorg(phl, 'cz'
                 ,abvcols = T))
  )
})
