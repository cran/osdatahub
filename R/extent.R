
#' Create extents from geometries
#'
#' Provide extents from various types of input features and geometries to be
#' used as filters in OS Data Hub API queries.
#' @param bbox A bounding box, passed as a numeric vector in (xmin, ymin, xmax,
#'   ymax) or a \code{data.frame} object with numeric columns.
#' @param polygon A polygon specified in a WKT string, an object of class
#'   \code{geos}, or an object of class \code{sf}.
#' @param geojson A character string defining a polygon in GeoJSON format.
#' @param grid_ref A character string with a British National Grid reference.
#'   The extent is formed by the grid square of the reference.
#' @param centre Either a numeric vector with coordinates in the form (x, y), a
#'   Point object in a WKT string, a Point as a \code{geos} geometry or an
#'   object of class \code{sf}.
#' @param radius (numeric) The radius of the circle in meters.
#' @param crs (character or numeric)  The identifier for coordinate reference
#'   system information for the feature, either in the format "epsg:xxxx" or an
#'   EPSG number. e.g. British National Grid can be supplied as "epsg:27700" or
#'   27700. Available CRS values are: EPSG:27700, EPSG:4326, EPSG:7405,
#'   EPSG:3857, and CRS84. Defaults to CRS84.
#' @param returnType (character) Define the object returned. The default is
#'   \code{'qExtent'} to define a "query extent" object expected internally by
#'   \code{osdatahub}. Other options are \code{'wkt'} to return the geometry in
#'   Well-Known Text format or \code{'geos'} to return an object of class
#'   \code{geos}.
#'
#' @details When defining an extent by a radius around a point, the CRS must be
#'   either 'epsg:27700' or 'epsg:3857' which implies the units of the distance
#'   for the radius are meters.
#'
#'   The \code{qExtent} return option identifies a simple class of objects
#'   containing a polygon of the extent in WKT format, the bounding box
#'   coordinates, and a CRS string. It is intended to be used internally by
#'   functions in \code{osdatahub}.
#'
#' @returns The coordinates of the polygon boundary as defined by
#'   \code{returnType}.
#'
#' @examples
#' extent_from_bbox(c(600000, 310200, 600900, 310900), "epsg:27700", returnType = 'wkt')
#'
#' extent_from_radius(c(441317, 112165), radius = 200)
#'
#' extent_from_bng("SU3715")
#'
#' @import geos
#' @name extent
#' @export
extent_from_bbox <- function(bbox,
                             crs = 'crs84',
                             returnType = c('qExtent', 'geos', 'wkt')){

  # Validate parameters.
  if(missing(bbox)){
    stop('Please provide a bounding box', call. = FALSE)
  } else{
    if(is.null(bbox)){
      stop('Please provide a bounding box.', call. = FALSE)
    }

    if(is.character(bbox)){
      stop('Please provide bounding box as a numeric vector or data frame.',
           call. = FALSE)
    }

    if(is.numeric(bbox)){
      if(length(bbox) < 4L){
        stop('Please provide xmin, ymin, xmax, and ymax coordinates.',
             call. = FALSE)
      }

      if(!is.null(names(bbox))){
        names(bbox) <- tolower(names(bbox))
        bbox <- bbox[c('xmin', 'ymin', 'xmax', 'ymax')]
      } else{
        # Assume the first four values.
        bbox <- bbox[1:4]
      }
    }

    if(is.data.frame(bbox)){
      if(nrow(bbox) > 1L){
        stop('Please provide only one bounding box.', call. = FALSE)
      }

      if(ncol(bbox) < 4L){
        stop('Please provide xmin, ymin, xmax, and ymax coordinates.',
             call. = FALSE)
      }

      # Get attributes.
      names(bbox) <- tolower(names(bbox))

      if(all(names(bbox) %in% c('xmin', 'ymin', 'xmax', 'ymax'))){
        bbox <- as.numeric(bbox[1L, c('xmin', 'ymin', 'xmax', 'ymax')])
      } else{
        # Assume the first four columns.
        bbox <- as.numeric(bbox[1L, 1:4])
      }
    }

    # Process geometry. xmin, ymin, xmax, ymax
    geom <- geos::geos_create_rectangle(bbox[1], bbox[2], bbox[3], bbox[4])
  }

  if(!valid_crs(crs)){
    stop('Please provide a valid CRS.', call. = FALSE)
  } else{
    crs <- get_crs(crs, returnType = 'code')
  }

  # Create a polygon.
  # Check `sf` package is available.
  if (!requireNamespace('sf', quietly = TRUE)){
    geometry <- NULL
  } else{
    nCRS <- get_crs(crs, 'number')
    if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.

    geometry <- sf::st_sf(geometry = sf::st_as_sfc(geom),
                          crs = sf::st_crs(nCRS))
  }

  returnType <- match.arg(returnType)

  # Extent of the polygon.
  geom <- geos::geos_envelope(geom)
  attr(geom, 'crs') <- crs

  # Process returns.
  if(returnType == 'qExtent'){
    return(make_extent(wkt = geos::geos_write_wkt(geom),
                       bbox = geos::geos_extent(geom),
                       polygon = geometry,
                       src = 'BBox',
                       crs = crs))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }
}


#' @name extent
#' @export
extent_from_polygon <- function(polygon,
                                crs = 'crs84',
                                returnType = c('qExtent', 'geos', 'wkt')){

  # Validate parameters.
  if(missing(polygon)){
    stop('Please provide a polygon object.', call. = FALSE)
  }

  if(is.null(polygon)){
    stop('Please provide a polygon object.', call. = FALSE)
  }

  if(is.character(polygon)){
    if(length(polygon) > 1L){
      stop('Please provide only one feature.', call. = FALSE)
    }

    if(nchar(polygon) == 0L){
      stop('Please provide a polygon object.', call. = FALSE)
    }

    # Assume this a WKT string.
    geom <- geos::geos_unnest(geos::geos_read_wkt(polygon))

  } else if(inherits(polygon, 'geos_geometry')){
    if(!is.null(attr(polygon, 'crs'))){
      crs <- attr(polygon, 'crs')
    }
    geom <- polygon

  } else if(inherits(polygon, c('sf', 'sfc'))){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    }

    if(nrow(polygon) > 1){
      stop('Please provide only one feature.', call. = FALSE)
    }

    if(sf::st_geometry_type(polygon) != 'POLYGON'){
      stop('Only Polygon geometries can be supplied.', call. = FALSE)
    }

    # Process `sf` geometry.
    geom <- geos::as_geos_geometry(sf::st_geometry(polygon))
    crs <- sf::st_crs(polygon)$epsg
    inputPolygon <- polygon
  }

  if(length(geom) > 1L){
    stop('Please provide only one polygon feature.', call. = FALSE)
  }

  if(geos::geos_type(geom) != 'polygon'){
    stop('Only Polygon geometries can be supplied.', call. = FALSE)
  }

  if(!valid_crs(crs)){
    stop('Please provide a valid CRS.', call. = FALSE)
  } else{
    crs <- get_crs(crs, returnType = 'code')
  }

  returnType <- match.arg(returnType)

  # Create a polygon.
  # Check `sf` package is available.
  if (!requireNamespace('sf', quietly = TRUE)){
    geometry <- NULL
  } else{
    nCRS <- get_crs(crs, 'number')
    if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.

    attr(geom, 'crs') <- NULL
    geometry <- sf::st_sf(geometry = sf::st_as_sfc(geom),
                          crs = sf::st_crs(nCRS))
  }

  # Extent of the polygon.
  # geom <- geos::geos_envelope(geom)
  # attr(geom, 'crs') <- crs

  # Process returns.
  if(returnType == 'qExtent'){
    return(make_extent(wkt = geos::geos_write_wkt(geom),
                       bbox = geos::geos_extent(geom),
                       polygon = geometry,
                       src = 'Polygon',
                       crs = crs))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }
}


#' @name extent
#' @export
extent_from_geojson <- function(geojson,
                                crs = 'crs84',
                                returnType = c('qExtent', 'geos', 'wkt')){
  # Validate parameters.
  if(missing(geojson)){
    stop('Please provide a GeoJSON feature.', call. = FALSE)

  } else{
    if(is.null(geojson)){
      stop('Please provide a GeoJSON feature.', call. = FALSE)
    }

    if(!is.character(geojson)){
      stop('Please provide the GeoJSON as a character string.', call. = FALSE)
    }

    if(length(geojson) > 1L){
      stop('Please provide only one GeoJSON feature.', call. = FALSE)
    }

    if(nchar(geojson) == 0L){
      stop('Please provide a GeoJSON feature.', call. = FALSE)
    }
  }

  if(!valid_crs(crs)){
    stop('Please provide a valid CRS.', call. = FALSE)
  } else{
    crs <- get_crs(crs, returnType = 'code')
  }

  returnType <- match.arg(returnType)

  # Define geometry.
  geom <- geos::geos_unnest(geos::geos_read_geojson(geojson))

  if(length(geom) > 1L){
    stop('Please provide only one GeoJSON feature.', call. = FALSE)
  }

  if(geos::geos_type(geom) != 'polygon'){
    stop('Only Polygon geometries can be supplied.', call. = FALSE)
  }

  # Create a polygon.
  # Check `sf` package is available.
  if (!requireNamespace('sf', quietly = TRUE)){
    geometry <- NULL
  } else{
    nCRS <- get_crs(crs, 'number')
    if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.

    attr(geom, 'crs') <- NULL
    geometry <- sf::st_sf(geometry = sf::st_as_sfc(geom),
                          crs = sf::st_crs(nCRS))
  }

  attr(geom, 'crs') <- crs

  # Extent of the polygon.
  # geom <- geos::geos_envelope(geom)

  # Process returns.
  if(returnType == 'qExtent'){
    return(make_extent(wkt = geos::geos_write_wkt(geom),
                       bbox = geos::geos_extent(geom),
                       polygon = geometry,
                       src = 'GeoJSON',
                       crs = crs))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }
}


#' @name extent
#' @export
extent_from_radius <- function(centre,
                               radius,
                               crs = 'epsg:27700',
                               returnType = c('qExtent', 'geos', 'wkt')){

  # Validate parameters.
  if(missing(centre)){
    stop('Please provide centre coordinates.', call. = FALSE)
  }

  if(is.null(centre)){
    stop('Please provide centre coordinates.', call. = FALSE)
  }

  if(is.character(centre)){
    if(length(centre) > 1L)
      stop('Please provide only one centre point geometry.', call. = FALSE)

    if(nchar(centre) == 0L)
      stop('Please provide centre coordinates.', call. = FALSE)

    # Assuming we have a WKT geometry, try to define a point.
    geom <- geos::geos_unnest(geos::geos_read_wkt(centre))

    if(length(geom) > 1L){
      stop('Please provide only one centre point geometry.', call. = FALSE)
    }

    if(geos::geos_type(geom) != 'point'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }

  } else if(is.numeric(centre)){
    if(length(centre) != 2){
      stop('Please provide one pair of (x, y) coordinates only.', call. = FALSE)
    }
    x <- centre[1L]
    y <- centre[2L]

    # Define geometry.
    geom <- geos::geos_make_point(x, y)

  } else if(inherits(centre, 'geos_geometry')){

    if(geos::geos_type(geom) != 'point'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }
    geom <- centre

  } else if(inherits(centre, c('sf', 'sfc'))){
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    }

    if(nrow(centre) > 1){
      stop('Please provide only one feature.', call. = FALSE)
    }

    if(sf::st_geometry_type(centre) != 'POINT'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }

    # Process `sf` geometry.
    geom <- geos::as_geos_geometry(sf::st_geometry(centre))
    crs <- sf::st_crs(centre)$epsg

  }else{
    stop('Invalid format for centre point.', call. = FALSE)
  }

  if(missing(radius)){
    stop('Please provide a search radius distance.', call. = FALSE)
  } else{
    if(!is.numeric(radius)) stop('Please provide radius as a number.',
                                 call. = FALSE)
    if(length(radius) > 1) stop('Please provide only one search radius.',
                                call. = FALSE)
    if(radius <= 0) stop('Radius must be larger than 0 meters.',
                         call. = FALSE)
  }

  if(is.numeric(crs)){
    crs <- crs[1L]
    if(!crs %in% c(27700, 3857)){
      stop('CRS must be one of 27700 or 3857', call. = FALSE)
    } else{
      crs <- get_crs(crs, returnType = 'code')
    }
  } else if(is.character(crs)){
    crs <- crs[1L]
    if(!tolower(crs) %in% c('epsg:27700', 'epsg:3857')){
      stop('CRS must be one of \'epsg:27700\' or \'epsg:3857\'', call. = FALSE)
    } else{
      crs <- tolower(crs)
    }
  }

  returnType <- match.arg(returnType)

  # Define circle as a buffer around the centre point.
  bparams <- geos::geos_buffer_params(quad_segs = 16,
                                      end_cap_style = "round",
                                      join_style = "round",
                                      mitre_limit = 5.0,
                                      single_sided = FALSE)

  geom <- geos::geos_buffer(geom,
                            distance = radius,
                            params = bparams)

  # Create a polygon.
  # Check `sf` package is available.
  if (!requireNamespace('sf', quietly = TRUE)){
    geometry <- NULL
  } else{
    nCRS <- get_crs(crs, 'number')
    if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.

    attr(geom, 'crs') <- NULL
    geometry <- sf::st_sf(geometry = sf::st_as_sfc(geom),
                          crs = sf::st_crs(nCRS))
  }

  # Extent of the circle.
  # geom <- geos::geos_envelope(geom)

  # Process returns.
  if(returnType == 'qExtent'){
    return(make_extent(wkt = geos::geos_write_wkt(geom),
                       bbox = geos::geos_extent(geom),
                       polygon = geometry,
                       src = 'Radius',
                       crs = crs))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }
}


#' @name extent
#' @export
extent_from_bng <- function(grid_ref,
                            returnType = c('qExtent', 'geos', 'wkt')){
  if(missing(grid_ref)){
    stop('Please provide a BNG grid reference.', call. = FALSE)
  }

  if(is.null(grid_ref)){
    stop('Please provide a BNG grid reference.', call. = FALSE)
  }

  if(length(grid_ref) > 1L){
    stop('Please provide only one BNG grid reference.', call. = FALSE)
  }

  if(nchar(grid_ref) == 0L){
    stop('Please provide a BNG grid reference.', call. = FALSE)
  }

  geom <- bng_to_geom(grid_ref, returnType = 'geos')
  crs <- attr(geom, 'crs')  # Should always be EPSG:27700.

  # Create a polygon.
  # Check `sf` package is available.
  if (!requireNamespace('sf', quietly = TRUE)){
    geometry <- NULL
  } else{
    nCRS <- get_crs(crs, 'number')
    if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.

    attr(geom, 'crs') <- NULL
    geometry <- sf::st_sf(geometry = sf::st_as_sfc(geom),
                          crs = sf::st_crs(nCRS))
  }

  returnType <- match.arg(returnType)

  # Process returns.
  if(returnType == 'qExtent'){
    return(make_extent(wkt = geos::geos_write_wkt(geom),
                       bbox = geos::geos_extent(geom),
                       polygon = geometry,
                       src = 'BNG',
                       crs = crs))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }
}


#' Retrieve an extent for ONS geographies
#'
#' @param ons_code (character) A single ONS code representing a statistical
#'   area.
#' @param returnType (character) Define the object returned. The default is
#'   \code{'qExtent'} to define a "query extent" object expected internally by
#'   \code{osdatahub}. Other options are \code{'wkt'} to return the geometry in
#'   Well-Known Text format or \code{'geos'} to return an object of class
#'   \code{geos}.
#' @details The Office for National Statistics (ONS) maintains a source of
#'   official geographies for the UK, such as county boundaries, electoral
#'   wards, parishes, and census output areas. These boundaries are commonly
#'   used for data analysis, particularly of socio-economic factors. A full list
#'   of available ONS geographies can be found here:
#'   \url{https://statistics.data.gov.uk:443/atlas/resource?uri=http://statistics.data.gov.uk/id/statistical-geography/K02000001}.
#'
#'   When returning a \code{geos} object, the coordinate reference system
#'   attribute will be set to CRS:84 by default and not to a full CRS
#'   definition.
#'
#'   The \code{qExtent} return option identifies a simple class of objects
#'   containing a polygon of the extent in WKT format, the bounding box
#'   coordinates, and a CRS string. It is intended to be used internally by
#'   functions in \code{osdatahub}.
#'
#' @returns The coordinates of the polygon boundary.
#'
#' @seealso [extent]
#'
#' @examples
#' \donttest{
#' ext <- extent_from_ons_code("E05002470", returnType = 'wkt')
#' }
#'
#' @import geos
#' @export
extent_from_ons_code <- function(ons_code,
                                 returnType = c('qExtent', 'geos', 'wkt')){

  returnType <- match.arg(returnType)

  # Query the ONS API.
  geom <- get_ons_geom(ons_code, 'geos')
  crs = attr(geom, 'crs')

  # Create a polygon.
  # Check `sf` package is available.
  if (!requireNamespace('sf', quietly = TRUE)){
    geometry <- NULL
  } else{
    nCRS <- get_crs(crs, 'number')
    if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.

    attr(geom, 'crs') <- NULL
    geometry <- sf::st_sf(geometry = sf::st_as_sfc(geom),
                          crs = sf::st_crs(nCRS))
  }

  # Extent of the polygon
  # geom <- geos::geos_envelope(geom)

  # Process returns.
  if(returnType == 'qExtent'){
    return(make_extent(wkt = geos::geos_write_wkt(geom),
                       bbox = geos::geos_extent(geom),
                       polygon = geometry,
                       src = 'ONS',
                       crs = crs))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }
}


#' Construct a \code{qExtent} object
#' @param wkt WKT text string describing the geometry.
#' @param bbox Numeric vector with BBox corner coordinates.
#' @param polygon Object of type \code{sf} with the query shape, or \code{NULL}.
#' @param src String describing the construction method.
#' @param crs String for the coordinate reference system.
#'
#' @returns Object of type \code{qExtent} which extends a \code{list}.
#'
#' @examples
#' wkt <- 'POLYGON ((560000 230000, 570000 230000, 570000 240000, 560000 240000, 560000 230000))'
#' bbox <- data.frame('xmin'=560000, 'ymin'=230000, 'xmax'=570000, 'ymax'=240000)
#' ext <- make_extent(wkt, bbox, NULL, 'test', crs = 'epsg:27700')
#'
#' @keywords internal
#' @noRd
make_extent <- function(wkt,
                        bbox,
                        polygon,
                        src,
                        crs = c('epsg:4326', 'epsg:27700',
                                'epsg:3857', 'crs84')){

  crs <- match.arg(crs)

  if(wkt == '' && is.null(bbox)){
    stop('Must provide either a Polygon extent or a bounding box.',
         call. = FALSE)
  }

  if(!missing(wkt)){
    stopifnot(is.character(wkt))
  } else{
    wkt <- ''
  }

  if(!missing(bbox)){
    stopifnot(inherits(bbox, 'data.frame') && ncol(bbox) == 4)
    names(bbox) <- c('xmin', 'ymin', 'xmax', 'ymax')

    if(wkt == ''){
      wkt <- geos::geos_create_rectangle(bbox$xmin, bbox$ymin,
                                         bbox$xmax, bbox$ymax,
                                         crs = crs)
    }
  } else{
    bbox <- NULL
  }

  if(missing(src)){
    src <- ''
  }

  # Create 'extent' object.
  dat <- list('wkt' = wkt,
              'bbox' = bbox,
              'polygon' = polygon,
              'crs' = crs,
              'src' = src)
  class(dat) <- c('qExtent', class(dat))

  return(dat)
}


#' Extract polygon object from \code{qExtent} object
#' @param x Extent object.
#'
#' @returns Polygon representation of the extent geometry in WKT format.
#'
#' @examples
#' ply <- poly_from_extent(extent_from_bng('TL63'))
#'
#' @noRd
#' @keywords internal
poly_from_extent <- function(x){
  stopifnot(inherits(x, 'qExtent'))

  # Extract the WKT representation.
  return(x[['wkt']])
}


#' Print a summary of a \code{qExtent} object
#' @param x Extent object.
#' @param ... Additional parameters (not currently used).
#' @details Called for its side effects of printing a well-formatted summary of
#'   the query extent object.
#' @returns Invisibly returns the extent object.
#'
#' @examples
#' print(extent_from_bng('TL63'))
#'
#' @noRd
#' @export
print.qExtent <- function(x, ...){
  cat(paste0('OS Data Hub Query Extent\nCreated from: ',
             x$src,
             '\n','Bounding box: ',
             paste(x$bbox, collapse = ' '),
             '\n','Coord. Ref. Sys.: ',
             x$crs))

  invisible(x)
}

