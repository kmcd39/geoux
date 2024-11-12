# boilderplate ---------------------------------------------------------------------------
rm(list = ls())
require(tidyverse)
# option setting
options(tigris_use_cache = TRUE)
qry.year <- 2022

# get all regions with 250+ pop -------------------------------------------

pops <-
  map_dfr(geox::rx$countyfp
          ,~tidycensus::get_acs(
            geography = 'county'
            ,state = substr(.x, 1,2)
            ,county = substr(.x, 3,5)
            ,year = qry.year
            ,survey = "acs5"
            ,variable = 'B01001_001'
          ))

pops <- pops %>%
  rename_with( ~tolower(.x)) %>%
  select(geoid, pop = estimate)

pops <- pops %>%
  left_join(geox::rx
            ,by=c('geoid' = 'countyfp'))

cz.pops <- pops %>%
  group_by(cz) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  filter(!is.na(cz))

cbsa.pops <-  pops %>%
  group_by(cbsa) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  filter(!is.na(cbsa))

rpops <- rbind(
  geox::region.reorg(cz.pops, 'cz'),
  geox::region.reorg(cbsa.pops, 'cbsa')
  )

rpops %>%
  filter(pop > 250e3) %>%
  map( ~ sum(is.na(.)))



# write ------------------------------------------------------------------------

usethis::use_data(rpops)
