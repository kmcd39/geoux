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


# get shapes -------------------------------------------------------------------

plcs <- tigris::places(state = 25) %>% rename_with(tolower)

cbsas <- tigris::core_based_statistical_areas() %>% rename_with(tolower)
cbsas <- cbsas %>% filter(grepl('MA', name, ignore.case = F))


# visual -----------------------------------------------------------------------

ggplot() +
  geom_sf(data = plcs
          ,aes(fill = geoid)) +
  geom_sf(data = cbsas, fill = NA
          ,aes(color = geoid)) +
  scale_fill_discrete(guide = 'none') +
  scale_color_discrete(guide = 'none')


# get overlap ------------------------------------------------------------------

devtools::load_all()
ov <- geox::get.spatial.overlap(plcs, cbsas,
                          'placefp', 'cbsafp')

ovsf <- geox::get.spatial.overlap(plcs, cbsas,
                                'placefp', 'cbsafp'
                                ,return.sf = T)
ovsf


#  visual check ----------------------------------------------------------------
ov <- ov %>%
  group_by(placefp) %>%
  filter(perc.area == max(perc.area)) %>%
  left_join(plcs) %>% st_sf()


ggplot() +
  geom_sf(data = ov
          ,aes(fill = cbsafp)
          ,color = 'white') +
  geom_sf(data = cbsas, fill = NA
          ,aes(color = cbsafp)) +
  scale_color_discrete(guide = 'none')

"
split.plc <- ov %>%
  filter(perc.area < .99)

split.plc %>%
  ggplot() +
  geom_sf(aes(fill = cbsafp)
          ,color = 'white') +
  geom_sf(data = filter(cbsas,
                        cbsafp %in%
                          c('14460', '49340'))
          , aes(fill = cbsafp)
          , alpha = .5
          ,color = 'white') +
  scale_color_discrete(guide = 'none') +
  visaux::bbox2ggcrop(split.plc)
# COORD sf can break rstudio viewer, but saved image confirms correctness
visaux::ragg.wrapper()
"
