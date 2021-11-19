#' functions that interface with the `tigris` library to query layers.
#'
#' The flow is: given a spatial layer, can query counties with county_subset, a thin
#' wrapper. Then other thin wrappers will call



# tigris wrappers --------------------------------------------------------------


#' county.subset
#'
#' Given an `sf` object, get all counties that intersect. The spatial filter will
#' adapt based on input, but can be thrown off when `x` isn't coterminous with
#' counties. In this case the function `xwalks::get.spatial.overlap` can be helpful.
#'
#' @param x `sf` or `bbox`
#' @param cos counties sf object. If none supplied, they are retrieved using
#'   `tigris` library
#' @inheritDotParams flexible.spatial.filter
#'
#' @export county.subset
county.subset <- function(x, cos = NULL, year = 2019, ...) {

  if(is.null(cos))
    cos <- tigris::counties(year = year) %>%
      rename_with(tolower)

  cos <- flexible.spatial.filter(x, cos, ...)

  return(cos)
}


#' tracts.from.sf
#'
#' From an `sf` object, gets tracts or block groups over overlapping counties.
#'
#' Note that they may still be trimmed to match supplied area, if `x` isn't
#' coterminous with counties.
#'
#' @param x sf object to get neighborhoods over
#' @param countyfps Alternative to x, countyfp (as 5-char characters) to get nhoods for.
#' @param query.fcn tigris query fcn: `tracts` or `block_groups`
#' @param ... passed onto `query.fcn`
#'
#' @export tracts.from.sf
tracts.from.sf <- function(x = NULL, countyfps = NULL,
                           query.fcn = tigris::tracts,
                           ...) {

  if(is.null(countyfps)) {
    cos <- county.subset(x)
    countyfps <- cos$geoid
  }

  # download
  .params <- list(...)

  .nhds <- map_dfr(countyfps,
                   ~do.call(query.fcn
                            , c(list( substr(.x, 1, 2)
                                     ,substr(.x, 3, 5))
                                , .params))) %>%
    rename_with(tolower)


  return(.nhds)
}



#' water.wrapper
#'
#' Gets water areas based on supplied countyfp codes and/or other spatial area.
#'
#' @param countyfps 5-character state/county fp codes. Retrieved using
#'   `county.subset` and supplied `x` argument if null.
#' @param x `sf` or `bbox` to get overlapping water areas for. Passed onto
#'   `county.subset` if no county fps are supplied. Also used to spatially subset
#'   water areas.
#' @param size.min Minimum size in m^2, after internal boundaries are resolved (if a
#'   water area is represented by multiple contiguous polygons)
#'
#' @return water areas for region.
#'
#' @export water.wrapper
water.wrapper <- function(countyfps = NULL, x = NULL, size.min = 5e6, ...) {

  if(is.null(countyfps)) {
    cos <- county.subset(x, ...)
    countyfps <- cos$geoid
  }

  # download water
  water <- map_dfr(countyfps,
                   ~tigris::area_water(state =
                                         substr(., 1, 2),
                                       county =
                                         substr(., 3, 5))
  )

  # union and explode water
  water <- st_union(water) %>% st_cast("POLYGON") %>% st_sf()

  # filter by size of union'd body
  water <- water %>% filter(as.numeric(st_area(.$geometry)) > size.min )

  if(!is.null(x)) {
    water <- transform.and.crop(water, x)
  }

  return(water)
}


#' parks.wrapper
#'
#' @param x object to derive overlapping statefps from, if statefps is left as null. Aiddtional
#' @param statefps statefps to get parks for
#'
#' @export parks.wrapper
parks.wrapper <- function(x = NULL, statefps = NULL, ...) {

  if(!is.null(statefps)) {
    cos <- county.subset(x, ...)
    statefps <- cos$statefp
  }

  parks <- map_dfr(statefps,
                   ~tigris::landmarks(.x, type = "area")
  )

  colnames(parks) <- tolower(colnames(parks))

  parks <- parks[grepl('Park|Cmtry', parks$fullname),]

  if(!is.null(x))
    parks <- transform.and.crop(parks, x)

  return(parks)
}


#' places.wrapper
#'
#' @param x object to derive overlapping statefps from, if statefps is left as null. Aiddtional
#' @param countyfps countyfps to get parks for
#'
#' @export places.wrapper
places.wrapper <- function(.countyfps = NULL, x = NULL, year = 2019,
                           ...) {

  if(is.null(.countyfps)) {
    cos <- county.subset(x, ...)
    .countyfps <- cos$geoid
  }
  .statefps <- substr(.countyfps, 1, 2)

  plcs <- map_dfr(.statefps,
                  ~tigris::places(.x, year = year),
                  ...)

  if(!is.null(x))
    plcs <- transform.and.crop(plcs, x)

  return(plcs)

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
#' @x `sf` object spanning area you want to filter `polys` to
#' @param polys polygons to get over area `x`.
#' @param subset.approach spatial filter approach; one of "intersects" or "crop".
#'   Intersection or cropping to bbox.
#'
flexible.spatial.filter <- function(x, polys,
                                    subset.approach = c('intersects', 'crop')) {

  require(sf)

  polys <- st_transform(polys, st_crs(x))

  # use given subset approach. Use crop if x is a bbox
  if(subset.approach[1] == "intersects" &
     !"bbox" %in% class(x)) {

    polys <-  st_point_on_surface(polys)
    sbgp <- st_intersects(polys, x)

    polys <- polys[lengths(sbgp) > 0, ]

  } else if(subset.approach[1] == "crop" |
            "bbox" %in% class(x))

    polys <- st_crop(polys, x)

  return(polys)
}


#' transform.and.crop
#'
#' Steps common to many of the wrappers if an sf object is supplied to one of the
#' tigris wrappers. Subsets and crops the input.
#'
#' @param basis.layer Layer or bbox to match with crs and crop
#'
transform.and.crop <- function(new.layer, basis.layer) {

  new.layer %>%
    st_transform(st_crs(basis.layer)) %>%
    st_crop(basis.layer)
}


