#' functions that interface with the `tigris` library to query layers.
#'
#' The flow is: given a spatial layer, can query counties with county_subset, a thin
#' wrapper. Then other thin wrappers will call



# tigris wrappers --------------------------------------------------------------


#' county.subset
#'
#' Given an `sf` object or bbox, get all counties that fall within polygon or bbox.
#'
#' @inheritParams flexible.spatial.filter
#' @param cos counties sf object. If none supplied, they are retrieved using
#'   `tigris`/census api
#' @param ... passed onto `tigris::counties`
#'
#'
#' @export county.subset
county.subset <- function(x, cos = NULL,
                          ...) {

  if(is.null(cos))
    cos <- tigris::counties(...) %>%
      rename_with(tolower)

  cos <- flexible.spatial.filter(x, cos)

  return(cos)
}


#' nbhds.from.sf
#'
#' From a bbox or sf object x, gets tracts or block groups over overlapping counties.
#' For areas that may be smaller than counties, suggest using st_bbox first.
#'
#' Note that they may still be trimmed to match supplied area, if `x` isn't
#' coterminous with counties.
#'
#' @inheritParams flexible.spatial.filter
#' @param .countyfps Alternative to x, countyfp (as 5-char characters) to get areas
#'   for.
#' @param query.fcn tigris query fcn: `tracts` or `block_groups`
#' @param ... passed onto `query.fcn`
#'
#' @export nbhds.from.sf
nbhds.from.sf <- function(x = NULL, .countyfps = NULL,
                           query.fcn = tigris::tracts,
                           ...) {

  if(is.null(.countyfps)) {
    cos <- county.subset(x)
    .countyfps <- cos$geoid
  }

  # download
  .params <- list(...)

  nhds <- purrr::map_dfr(.countyfps,
                   ~do.call(query.fcn
                            , c(list( substr(.x, 1, 2)
                                     ,substr(.x, 3, 5))
                                , .params))) %>%
    rename_with(tolower)

  if(!is.null(x))
    nhds <- flexible.spatial.filter(x, nhds)


  return(nhds)
}


#' places.wrapper
#'
#' Gets parks in a state, based on census bureau data. Can query from sf object x or
#' state fp codes.
#'
#' @inheritParams flexible.spatial.filter
#' @param .statefps Alternative to x, statefp (as 2-char characters) to get areas
#'   for.
#' @param ... passed onto `tigris::places`
#'
#' @export places.wrapper
places.wrapper <- function(x = NULL, .statefps = NULL,
                           ...) {

  if(is.null(.statefps)) {
    cos <- county.subset(x)
    .countyfps <- cos$geoid
    .statefps <- substr(.countyfps, 1, 2) %>% unique()
  }

  plcs <- map_dfr(.statefps,
                  ~tigris::places( state = .
                                   , ...
                  ),
                  ...) %>%
    rename_with(tolower)

  if(!is.null(x))
    plcs <- flexible.spatial.filter(x, plcs)

  return(plcs)

}


#' parks.wrapper
#'
#' Gets parks in a state, based on census bureau data. Can query from sf object x or
#' state fp codes.
#'
#' @inheritParams parks.wrapper
#'
#' @export parks.wrapper
parks.wrapper <- function(x = NULL, .statefps = NULL,
                          ...) {

  if(is.null(.statefps)) {
    cos <- county.subset(x)
    .countyfps <- cos$geoid
    .statefps <- substr(.countyfps, 1, 2) %>% unique()
  }

  parks <- purrr::map_dfr(.statefps,
                   ~tigris::landmarks(.x
                                      , type = "area"
                                      , ...)
                   , ...) %>%
    rename_with(tolower)

  parks <- parks[grepl('Park|Cmtry', parks$fullname),]

  if(!is.null(x))
    parks <- flexible.spatial.filter(x, parks)

  return(parks)
}


#' water.wrapper
#'
#' Gets water areas based on supplied countyfp codes and/or other spatial area.
#'
#' @inheritParams nbhds.from.sf
#' @param size.min Minimum size in m^2, after internal boundaries are resolved (if a
#'   water area is represented by multiple contiguous polygons)
#'
#' @return water areas for region.
#'
#' @export water.wrapper
water.wrapper <- function(x = NULL, .countyfps = NULL,
                          size.min = 5e6, ...) {

  if(is.null(.countyfps)) {
    cos <- county.subset(x)
    .countyfps <- cos$geoid
  }

  # download water
  water <- purrr::map_dfr(.countyfps,
                   ~tigris::area_water(state =
                                         substr(., 1, 2),
                                       county =
                                         substr(., 3, 5),
                                       ...)
                   , ...)

  # union and explode water
  water <- st_union(water) %>%
    st_cast("POLYGON") %>%
    st_sf(geometry = .) %>%
    mutate(water.area =
             st_area(geometry))

  # filter by size of union'd body
  water <- water %>%
    filter(as.numeric(water.area) >= size.min )

  if(!is.null(x)) {
    water <- flexible.spatial.filter(x, water)
  }

  return(water)
}

# helpers ----------------------------------------------------------------------

#' flexible.spatial.filter
#'
#' Gets all polygons `polys` that overlap with sf object `x`.
#'
#' May use different subset approaches (intersects or crop), depending on whether you
#' want to match a polygon's area or its bounding box.
#'
#' If `x` isn't already points, it is transformed to them using
#' `st_points_on_surface`.
#'
#' Currently this doesn't work with non-coterminous polys. In this case, use
#' `xwalks::get.spatial.overlap`, and filter by degree of overlap.
#'
#' @param x `sf` or `bbox` object spanning area you want to filter polys to. Will use
#'   get intersection if sf object or crop if bbox.
#' @param polys polygons to get over area `x`.
#' @param subset.approach spatial filter approach; one of "intersects" or "crop".
#'   Intersection or cropping to bbox.
#'
flexible.spatial.filter <- function(x, polys) {

  require(sf)

  polys <- st_transform(polys, st_crs(x))

  # use given subset approach. Use crop if x is a bbox
  if(!"bbox" %in% class(x)) {

    pts <-  st_point_on_surface(polys)
    sbgp <- st_intersects(pts, x)
    polys <- polys[lengths(sbgp) > 0, ]

  } else if("bbox" %in% class(x))
    polys <- st_crop(polys, x)

  return(polys)
}



