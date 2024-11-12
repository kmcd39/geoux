# duplicated from xwalks package



#' get.spatial.overlap
#'
#' Calculates the % overlap between two geographies. Takes two sf objects, a
#' unique/row identifier column for each; returns a row for each combination of rows
#' in the input sfs that intersect with one another with a column for the percent
#' overlap. % overlap can indicate true overlap, or it can be an artifact of
#' differing resolutions between the shapefiles or other artifacts of their
#' representation. Filtering to geometries below a very small % overlap using
#' \code{filter.threshold} or otherwise can remove these slivers.
#'
#' @param sf1 First sf object
#' @param sf2 Second sf object
#' @param sf1.identifier colname as string for region identifiers for first sf object
#' @param sf2.identifier colname as string for region identifiers for second sf
#'   object
#' @param filter.threshold percent overlap between sf1 and sf2, as decimal, below
#'   which to trim results before returning.
#' @param return.sf Return sf object with geometries. Slows down operation to do so.
#' @param crs crs to use during spatial overlap calculations. Defaults to EPSG 5070,
#'   an equal-area projection.
#'
#' @import sf
#'
#' @export get.spatial.overlap
get.spatial.overlap <- function(sf1, sf2,
                                sf1.identifier, sf2.identifier
                                , filter.threshold = 0.01
                                , return.sf = F
                                , crs = 5070) {
  require(dplyr)
  require(sf)

  # browser()

  # set to long-lat crs
  sf1 <- sf1 %>% st_transform(crs)
  sf2 <- sf2 %>% st_transform(crs)

  # add areas to each sf object
  sf1$area_1 <- st_area(sf1)
  sf2$area_2 <- st_area(sf2)
  # get intersecting areas
  intersection <- st_intersection(sf1,
                                  sf2)

  # just keep geometries, if other geometry types were created in intersect
  # group appropriately & calculate intersections / whole
  intersection <- intersection %>%
    st_collection_extract("POLYGON")

  # calculate areas
  intersection$int.area <-
    st_area(intersection$geometry)

  # % sf 1 contained in intersection area
  overlap.index <- intersection %>%
    mutate(perc.area =
             as.numeric(int.area) /
             as.numeric(area_1) )

  if(!return.sf)
    overlap.index <- tibble(overlap.index)

  # When one area is a multipolygon, the areal overlap might be split between two
  # rows. Group by & sum to combine these rows
  overlap.index <- overlap.index %>%
    group_by_at(c(sf1.identifier, sf2.identifier)) %>%
    summarise(perc.area = sum(perc.area)) %>%
    ungroup()

  # filter based on % size minimum and retain only IDs
  overlap.index <- overlap.index %>%
    filter(perc.area > filter.threshold) %>%
    select(c(all_of(sf1.identifier),
             all_of(sf2.identifier),
             "perc.area"))

  return(overlap.index)
}



#' generate.coterminous.xwalk
#'
#' If two regions are known to be co-terminous, this will generate a xwalk
#' between them. This method is more reliable than other obvious methods, which
#' can be thrown off by irregular shapes or differing resolutions between the
#' spatial datasets. Put the smaller region first. Will return 1 row/smaller
#' geo.
#' @param smaller.geo sf object containing the smaller geometries. (Because this
#'   is for coterminous regions, 1 set should be consistently smaller or the
#'   same size as the other)
#' @param larger.geo sf object containing larger geos.
#' @param trim.smaller Whether or not to trim smaller geos to combined area of
#'   all larger ones. Necessary, for example, if larger geos are clipped to
#'   water lines or otherwise trimmed, and the smaller ones are not. (In these
#'   cases, the smaller geometry is known to be smaller, but its spatial
#'   representation may not actually be). This step can be very computationally
#'   expensive for larger areas
#' @import sf dplyr
#' @export
generate.coterminous.xwalk <- function(smaller.geo, larger.geo, keep.geometry = T,
                                       trim.smaller = F) {
  require(sf)
  require(dplyr)

  # ensure identical crs
  xcrs = st_crs(smaller.geo)
  if(st_crs(larger.geo) != xcrs) {
    cat("\nTransforming sf in second argument to new crs\n")
    larger.geo <- st_transform(larger.geo, xcrs)
  }
  #
  if(trim.smaller)
    smaller.geo <- st_intersection(smaller.geo # might be more efficient way of doing this
                                   , st_union(larger.geo))

  pts <- st_point_on_surface(smaller.geo)
  out <- st_join(pts, larger.geo)

  if(keep.geometry)
    return(out)

  out <- tibble(out) %>% select(-geometry)
  return(out)
}
