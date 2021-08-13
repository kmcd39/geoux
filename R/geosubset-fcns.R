
#' geosubset
#'
#' Subsets a dataframe with one column representing tracts, counties, census block
#' groups, or another areal unit with STATE+COUNTY fips codes comprising the first
#' 5-characters of their geoid (as detailed here
#' https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html).
#'
#' Can take cz or cbsa or place to subset to, and use one of the xwalks in this
#' package to determine correspondence. In this case, attention should be payed to
#' the year of the areal unit.
#'
#' @param x df with at least 1 column containing state+county geoid hierarchy
#' @param subset.cols columns to subset to. `geoid` by default; `c('origin','dest')`
#'   may be useful for mobility data
#' @param cz_name,cz,countyfp,cbsa_id,plc_id Region/county/place to subset to
#' @inheritParams x2cos
#' @inheritParams plc2co
#'
#' @return df `x`, subsetted to the specified region.
#'
#' @export geosubset
geosubset <- function(x, subset.cols = c("geoid")
                      , ...) {
  require(tidyverse)
  .countyfp <- x2cos(...)
  x %>%
    filter(across(all_of(subset.cols),
                  ~(substr(.x, 1, 5)
                    %in% .countyfp)))
}

#' x2cos
#'
#' Given a cz,cbsa, or place identifier/geoid, gets all counties (5-character fp
#' codes) that overlap given region. countyfps correspond with 2015 counties.
#'
#' @param cz_name,cz,countyfp,cbsa_id,plc_id Identifier for region to get county
#'   codes for.
#'
#' @return List of 5-char countyfp codes overlapping with region.
#'
#' @export
x2cos <- function(cz_name = NULL, cz = NULL,
                  countyfp = NULL,
                  cbsa = NULL, plc_id = NULL) {

  argns <- as.list(environment())
  non.null <- map_lgl(argns,
                     ~!is.null(.))
  # ensure 1 non-null arg
  if(1 != sum(non.null))
    stop("x2cos needs 1 non-null arg (multiple or 0 are NULL")

  # get column based on non-null argument
  col <- names(argns[non.null])
  i <- argns[non.null]

  if(! col %in% 'plc_id' )
    .countyfp <- rx %>%
    filter(get(col) %in% i) %>%
    pull(countyfp)
  else {
    .countyfp <- plc2co(plc_id)
  }

  return(.countyfp)
}



#' plc2co
#'
#' Helper fcn called from `x2cos` when a plc_id is provided
#'
#' @inheritParams x2cos
#' @param overlap.threshold If a CBG has >= this % inside a Place boundaries, it will
#'   be included.
#'
plc2co <- function(plc_id,
                   overlap.threshold = 50) {

  requireNamespace('xwalks')
  cbg2plc <- xwalks::cbg2plc

  cbgs_in_place <- cbg2plc %>%
    filter(plc %in% plc_id &
             perc.overlap >=
             overlap.threshold) %>%
    pull(cbg)

  .countyfp <- substr(cbgs_in_place, 1, 5) %>% unique()

  return(.countyfp)
}
