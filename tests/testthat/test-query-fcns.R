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

cofps <- geox::x2cos(cz = '20100')
cosf <- geox::county.subset(x = czsf)

cofps
head(cosf)

testthat::test_that("spatial query counties", {
  testthat::expect_equal(sort(cofps)
                         , sort(cosf$geoid))
})

cosf[2] %>% plot()

# tracts -----------------------------------------------------------------------


ctsf <- geox::tracts.from.sf(x = czsf)
bgsf <- geox::tracts.from.sf(x = czsf
                             ,query.fcn = tigris::block_groups
                             ,cb = T
                             ,year = 2018
                             )

ctsf['countyfp'] %>% plot()
bgsf['countyfp'] %>% plot()

# for just portland's county
portlandco <- cosf %>% filter(grepl('Cumberland', name))
pctsf <- geox::tracts.from.sf(x = portlandco)
pbgsf <- geox::tracts.from.sf(x = portlandco
                             ,query.fcn = tigris::block_groups
                             ,cb = T
                             ,year = 2018)


pctsf['aland'] %>% plot()
pbgsf['aland'] %>% plot()

# bbox crop -- very cool :)
pbbgsf <- geox::tracts.from.sf(x = st_bbox(portlandco)
                              ,query.fcn = tigris::block_groups
                              ,cb = T
                              ,year = 2018)
pbbgsf['aland'] %>% plot()

# places -----------------------------------------------------------------------

library(mapview)
devtools::load_all()

plcsf.r <- geox::places.wrapper(x = st_bbox(czsf)
                              , year = 2020
                              )
plcsf.r %>% nrow()
plcsf.r %>% distinct() %>% nrow()

plcsf.old <-  geox::places.wrapper(x = st_bbox(czsf)
                                   , year = 2011
                                   )


plcsf.r['aland'] %>% plot()
plcsf.old['aland'] %>% plot()

czsf %>% mapview::mapview() + mapview::mapview(plcsf)


# parks ------------------------------------------------------------------------

parksf <- geox::parks.wrapper(x = portlandco
                              , year = 2019)

parksf

parksfi <- geox::parks.wrapper(x = portlandco
                              , year = 2019
                              , subset.approach = 'intersects'
                              )

parksf['aland'] %>% plot()# + mapview(st_boundary(portlandco))
parksfi['aland'] %>% plot()
