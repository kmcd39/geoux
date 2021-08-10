#' bundles selected xwalks
#'
library(sf)
library(tidyverse)

# option setting
sf_use_s2(F)
options(tigris_use_cache = TRUE)
Sys.setenv("VROOM_SHOW_PROGRESS"="false")

# counties, czs, cbsas ---------------------------------------------------------

rx <- xwalks::co2cz %>%
  left_join(xwalks::co2cbsa)

rx$countyfp %>% unique() %>% length()
rx$countyfp %>% length()

usethis::use_data(rx, overwrite = TRUE)





