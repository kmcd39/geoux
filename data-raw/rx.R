#' bundles selected xwalks
#'
library(sf)
library(tidyverse)

# option setting
qry.year <- 2022
#sf_use_s2(T)
options(tigris_use_cache = TRUE)
Sys.setenv("VROOM_SHOW_PROGRESS"="false")

# co2cbsa ----------------------------------------------------------------------

cbsas <- tigris::core_based_statistical_areas(year = qry.year)
counties <- tigris::counties(year = qry.year)

counties %>% tibble() %>% count(CBSAFP)

cbsas <- cbsas %>% select(cbsa = CBSAFP, cbsa_name = NAME, geometry)
counties <- counties %>% select(statefp = STATEFP, county = COUNTYFP, geometry)
co2cbsa <- xwalks::generate.coterminous.xwalk(counties, cbsas, keep.geometry = F)

co2cbsa$countyfp = with(co2cbsa,
                        paste0(statefp, county))


co2cbsa <- co2cbsa %>%
  filter(!is.na(cbsa)) %>%
  select(-county) %>%
  distinct() %>%
  select(statefp, countyfp, cbsa, cbsa_name) %>%
  arrange(cbsa)

co2cbsa

# counties, czs, cbsas ---------------------------------------------------------

# previous version has CZs too.

# rx <- xwalks::co2cz %>%
#   left_join(xwalks::co2cbsa)
#
# rx$countyfp %>% unique() %>% length()
# rx$countyfp %>% length()


# write -------------------------------------------------------------------

usethis::use_data(rx, overwrite = TRUE)





