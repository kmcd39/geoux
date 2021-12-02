library(tidyverse)
library(geox)


# test xwalks ------------------------------------------------------------------


testthat::test_that("philly counties in PA/NJ", {
  testthat::expect_true(
  unique(substr(x2cos(cz = '19700')
         ,1,2) %in% c('34', "42"))
  )
})




# test with multiple czs -------------------------------------------------------

# (visual test)
library(sf)
sf_use_s2(T)
options(tigris_use_cache = TRUE)

# multiple CZs
mczs <- c("24900", "24701")


cos <- tigris::counties()
cos %>%
  filter(GEOID %in%
                 x2cos(cz = mczs)) %>%
  mapview::mapview()# +
  {geox::build.CZs(.czs = c("24900", "24701")) %>%
      mapview::mapview()}

x2cos(cz = mczs)


# spatial tests ----------------------------------------------------------------

# spatial setup
library(sf)
sf_use_s2(T)
options(tigris_use_cache = TRUE)
Sys.setenv("VROOM_SHOW_PROGRESS"="false")

pops <-
  tidycensus::get_acs('tract'
                      ,variables = 'B01001_001'
                      ,year = 2019
                      ,state = 42) %>%
  rename_with(tolower)

pops

pops
rx %>% filter(statefp == '42')

rx %>% filter(statefp == '42')

# harrisburg
tmp <- geosubset(pops
                 ,'geoid'
                 ,cz = '19200')

# PA-portion of Philly cbsa
tmp <- geosubset(pops, cbsa = '37980')

tmp

# attach geos
tmpsf <- attach.geos(tmp)
tmpsf['estimate'] %>% plot()


# cz/cbsa subset ---------------------------------------------------------------

testthat::test_that("cz harrisburg subset", {
  testthat::expect_true(
    all(x2cos(cz_name = "Harrisburg") %in%
          {xwalks::co2cz %>% filter(cz_name == 'Harrisburg') %>% pull(countyfp)})
  )
})

rx %>% filter(grepl("Phila", cz_name))
testthat::test_that("cbsa phila subset", {
  testthat::expect_true(
    all(x2cos(cbsa = "37980") %in%
          {xwalks::co2cbsa %>%
              filter(cbsa == "37980") %>% pull(countyfp)})
  )
})


# plc subset -------------------------------------------------------------------

salem <- xwalks::plc.co.cz %>%
  filter(grepl('Winston-Salem', cz_name))

salem.counties <- xwalks::cbg2plc %>%
  filter(plc == '3775000') %>%
  pull(cbg) %>% substr(1,5) %>% unique()


testthat::test_that("plc salem subset", {
  testthat::expect_true(
    all(x2cos(plc_id = '3775000') %in%
          salem.counties))
})



