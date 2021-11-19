# ---------------------------------------------------------------------------
rm(list = ls())
require(tidyverse)
require(sf)
#library(geox)

# option setting
sf_use_s2(T)
options(tigris_use_cache = TRUE)

# dropbox dir or della base dir
ddir <- Sys.getenv('drop_dir')


devtools::load_all()


# get a test region ------------------------------------------------------------

arrs <- geox::rpops %>%
  filter(pop > 50e3
         ,rt == 'cz') %>%
  add.rns()

arrs %>%
  filter(pop > 500e3) %>%
  arrange(pop) %>%
  head(100) %>%
  pull(rn)

arrs %>%
  filter(grepl('Portland', rn))

czsf <- geox::build.CZs('20100')


# test tigris fcns -------------------------------------------------------------


# cos --------------------------------------------------------------------------

test
cofps <- geox::x2cos(cz = '20100')
cosf <- geox::county.subset(x = czsf)

testthat::test_that("spatial query counties", {
  testthat::expect_equal(sort(cofps)
                         , sort(cosf$geoid))
})


# tracts -----------------------------------------------------------------------

cts <- geox::tracts.from.sf(x = czsf)


