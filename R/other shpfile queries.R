#' These functions use non-tigris queries that might be more specialized, but I like
#' for my personal convenience.

#' get.NHPN
#'
#' Get National Highway Planning Network highway data.
#'
#' @param sfx if not NULL, an `sf` object to use to crop hwy data. Will also
#'   transform hwys to match CRS
#' @param dropbox.dir,subdir concatenated to get to nhpn data
#'
#' @export get.NHPN
get.NHPN <- function(sfx = NULL,
                     dropbox.dir = Sys.getenv("drop_dir"),
                     subdir = "shapefiles/nhpn/") {
  require(sf)
  require(dplyr)

  nhpn.dir <-
    paste0(dropbox.dir,
           subdir)
  if(!is.null(sfx)) {
    wkt.str <- sfx %>%
      st_union() %>%
      st_as_text()

    hwys <- nhpn.dir %>%
      list.files(pattern = "shp$",
                 full.names = T) %>%
      st_read(wkt_filter = wkt.str) %>%
      st_transform(st_crs(sfx))
  } else {
    hwys <- nhpn.dir %>%
      list.files(pattern = "shp$",
                 full.names = T) %>%
      st_read()
  }
  colnames(hwys) <- tolower(colnames(hwys))

  return(hwys)
}



# open street map query ---------------------------------------------------


#' osm.query
#'
#' Extracts bbox from supplied base sf, formats for osm api and extracts
#' requested features.
#' @param basesf sf object to query osm over bbox of
#' @param features osm features to request. See \code{?add_osm_feature} or
#'   https://wiki.openstreetmap.org/wiki/Map_Features
#' @examples
#' \dontrun{phr <- osm.query(divDat::czs[1,], "railway") }
osm.query <- function(basesf, features) {
  require(sf)

  bboxstr <-
    matrix(
      st_bbox(st_transform(basesf, 4326)), nrow = 2,
      dimnames = list( c("x","y")
                       ,c("min","max"))
    )

  osm_sf <- osmdata::opq(bboxstr) %>%
    osmdata::add_osm_feature(features) %>%
    osmdata::osmdata_sf

  return(osm_sf)
}
