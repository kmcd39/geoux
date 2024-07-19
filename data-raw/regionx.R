library(tidyverse)
library(sf)


# pull regions, states and census divisions -------------------------------
statesf <- tigris::states(year = 2022)
#statesf %>% mapview::mapview()

regionx <- tigris::states(year = 2022) %>%
  tibble() %>%
  rename_with(tolower) %>%
  select(region, division, statefp, stusps, state = name)

regionx



## pull regions/divs for names ---------------------------------------------

divs <- tigris::divisions(year = 2022) %>%
  tibble() %>%
  rename_with(tolower)  %>%
  select(div.geoid = geoid, division.name = name)

regions <- tigris::regions(year = 2022) %>%
  tibble() %>%
  rename_with(tolower)  %>%
  select(reg.geoid = geoid, region.name = name)

regionx <- regionx %>%
  left_join(divs
            , by = c("division" = "div.geoid")
            ) %>%
  left_join(regions
            , by = c("region" = "reg.geoid")
            )

regionx <- regionx %>%
  select(matches("state|stusps"), matches("region"), matches("division"))

# write -----------------------------------


## code to prepare `regionx` dataset goes here

usethis::use_data(regionx, overwrite = TRUE)
