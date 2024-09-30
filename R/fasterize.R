make_sf <- function(x, attr = NULL) {
    structure(list(geom = x), names = c("geom"), row.names = seq_len(length(x)), 
              sf_column = "geom", class = c("sf", "data.frame" ))
}
  
geom_from_terra <- function(x) {
  geom <- try(wk::wkb(x@ptr$wkb_raw()), silent = TRUE)
  geom
}

field_from <- function(x, field) {
  if (!inherits(x, "data.frame") && !inherits(x, "SpatVector")) return(NULL)
  if (is.null(field)) return(NULL)
  unlist(x[[field[1L]]])
}

sfgeom_from <- function(x) {
  if (inherits(x, "SpatVector")) {
    x <- geom_from_terra(x)
  }
  geom <- wk::wk_handle(x, wk::sfc_writer())
  
  geom
}
   
  
#' Rasterize a vector or dataframe object of polygons
#'
#' Rasterize set of polygons
#'
#' This is a high-performance replacement for [raster::rasterize()].
#'
#' The algorithm is based on the method described in course materials provided
#' by [Wayne O. Cochran](https://labs.wsu.edu/wayne-cochran/). The algorithm
#' is originally attributed to
#' Wylie et al. (1967) \doi{10.1145/1465611.1465619}. 
#'
#' Note that original implementation worked only for sf dataframes of class "sf", but this 
#' now works for any polygon vector (sfc, wkt, wkb, geos) or dataframe with a polygon vector
#' supported by the wk package handlers. 
#' 
#' @param sf a polygon vector or data frame object with a geometry column of POLYGON and/or
#' MULTIPOLYGON (equivalent) objects.
#' @param raster A raster  object. Used as a template for the raster output.
#' Can be created with [raster::raster()].
#' The fasterize package provides a method to create a raster object from
#' an polygon dataset.
#' @param field character (or numeric vector). The name of a column in `sf`,
#' providing a value for each of the polygons rasterized. If NULL (default),
#' all polygons will be given a value of 1.  If a numeric vector this value
#' will be used as the value given to the pixel.   (No recycling is done). 
#' @param fun character. The name of a function by which to combine overlapping
#' polygons. Currently takes "sum", "first", "last", "min", "max", "count", or
#' "any".  Future versions may include more functions or the ability to pass
#' custom R/C++ functions. If you need to summarize by a different function,
#' use `by=` to get a RasterBrick and then [raster::stackApply()] or
#' [raster::calc()] to summarize.
#' @param background numeric. Value to put in the cells that are not covered by
#' any of the features of x. Default is NA.
#' @param by character.  The name of a column in `sf` by which to aggregate
#' layers.  If set, fasterize will return a RasterBrick with as many layers
#' as unique values of the `by` column.
#' @return A raster of the same size, extent, resolution and projection as the
#' provided raster template.
#' @references Wylie, C., Romney, G., Evans, D., & Erdahl, A. (1967).
#'   Half-tone perspective drawings by computer. Proceedings of the November
#'   14-16, 1967, Fall Joint Computer Conference. AFIPS '67 (Fall).
#'   \doi{10.1145/1465611.1465619}
#' @examples
#' library(wk)
#' library(fasterize)
#' p123 <- c(paste0("POLYGON ((-180 -20, -140 55, 10 0, -140 -60, -180 -20),", 
#'                  "(-150 -20, -100 -10, -110 20, -150 -20))"), 
#'             "POLYGON ((-10 0, 140 60, 160 0, 140 -55, -10 0))", 
#'             "POLYGON ((-125 0, 0 60, 40 5, 15 -45, -125 0))")
#' pols <- data.frame(value = seq_along(p123), geometry = wk::as_wkt(p123))
#' ex <- as.numeric(wk_bbox(pols))[c(1, 3, 2, 4)]
#' r <- raster::raster(raster::extent(ex), res = 1)
#' r <- fasterize(pols, r, field = "value", fun="sum")
#' plot(r)
#' 
#' pols$group <- c("g1", "g1", "g2")
#' rg <- fasterize(pols, r, field  = pols$value, by = "group")
#' @export
fasterize <- function(sf, raster, field = NULL, fun = "last", background = NA_real_, by = NULL) {
  fieldwasnull <- is.null(field)
  bywasnull <- is.null(by)
  
  geom <- sfgeom_from(sf)
  
  if ((is.numeric(field) && length(field) == length(geom) )) {
    ## do nothing
  } else {
    field <- field_from(sf, field)  ## at this point we have a vector of values, same length as geom, or NULL
  }
  if (length(by) == length(geom) ) {
    ## do nothing
  } else {
    by <- field_from(sf, by)  ## at this point we have a vector of values, same length as geom, or NULL
  }
  ## check the types up here
  types <- wk::wk_meta(sf)$geometry_type
  bad <- ! types %in% c(3, 6)


  if (any(bad)) {
    if (all(bad)) stop("no polygon geometries to fasterize")
    geom <- geom[!bad, ]
    field <- field[!bad]
    message(sprintf("removing %i geometries that are not polygon or multipolygon equivalent", sum(bad)))
  }

  x1 <- make_sf(geom)
  if (length(field) == dim(x1)[1L]) {
      x1[["field"]] <- field
      field <- "field"
  }
  if (length(by) == dim(x1)[1L]) {
      x1[["by"]] <- by
      by <- "by"
  }
  if (!fieldwasnull && is.null(field)) stop("'field' not found in data")
  if (!bywasnull && is.null(by)) stop("'by' not found in data")
  
  fasterize_cpp(x1, raster, field, fun, background, by)
}

