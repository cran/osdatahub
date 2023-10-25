
#' Return the geometry of a British National Grid square
#'
#' Convert a valid British National Grid (BNG) grid reference string into a grid
#' square with the resolution implied by the length of the reference string.
#' @param grid_ref (character) BNG grid reference (required).
#' @param returnType (character) Representation for the returned geometry.
#'   Choose \code{'wkt'} or \code{'geojson'} to return the geometry in
#'   Well-Known Text format or GeoJSON, respectively, \code{'geos'} to return an
#'   object of class \code{geos} or \code{'sf'} for a Simple Features object of
#'   class \code{sf}. Default is WKT format.
#' @details The National Grid is a unique reference system that covers Great
#'   Britain in a series of grid squares at multiple scales. Grid references
#'   begin with 2 letters to identify 100km squares followed by a series of
#'   digits to identify quadrants nested within. For more information, see
#'   \url{https://www.ordnancesurvey.co.uk/documents/resources/guide-to-nationalgrid.pdf}
#'
#'   The purpose of this function is to generate geometries based on the extent
#'   of the grid square which can be used as spatial filters in OS Data Hub API
#'   queries.
#'
#'   Note that all geometries returned will have a coordinate reference system
#'   (CRS) of a EPSG:27700. The \code{sf} package must be installed in order to
#'   return an object of class \code{sf}.
#'
#' @returns The coordinates of the grid square boundary in either Well-Known
#'   Text (WKT) format, GeoJSON format, an object of class \code{geos} or as a
#'   Simple Features object of class \code{sf}.
#'
#' @seealso [extent_from_bng()]
#'
#' @examples
#' bng_to_geom('TL63')
#' bng_to_geom('TL683365', returnType = 'geojson')
#'
#' @import geos
#' @export
bng_to_geom <- function(grid_ref,
                        returnType = c('wkt', 'geojson', 'geos', 'sf')){
  if(missing(grid_ref)){
    stop('Please provide a BNG grid reference.', call. = FALSE)
  }

  valid_grid_ref(grid_ref)
  returnType <- match.arg(returnType)

  grid_ref <- toupper(grid_ref)
  grid_ref <- gsub(' ', '', grid_ref, fixed = TRUE)

  # First set of eastings and northings.
  x1 <- PREFIX1[PREFIX1$prefix == substr(grid_ref, 1, 1), 'x']
  y1 <- PREFIX1[PREFIX1$prefix == substr(grid_ref, 1, 1), 'y']

  x2 <- PREFIX2[PREFIX2$prefix == substr(grid_ref, 2, 2), 'x']
  y2 <- PREFIX2[PREFIX2$prefix == substr(grid_ref, 2, 2), 'y']

  # Second part of grid reference offsets
  if(nchar(grid_ref) > 2L){
    suffix <- substr(grid_ref, 3, nchar(grid_ref))
    l <- (nchar(suffix) / 2)

    sx <- as.numeric(substr(suffix, 1, l)) * 10^(5 - l)
    sy <- as.numeric(substr(suffix, l + 1, nchar(suffix))) * 10^(5 - l)

  } else{
    sx <- 0
    sy <- 0
  }

  # Set easting, northing of lower-left corner.
  e <- (100000 * (x1 + x2)) + sx
  n <- (100000 * (y1 + y2)) + sy

  # Construct grid square.
  resolution <- get_bng_resolution(grid_ref)
  # xmin, ymin, xmax, ymax
  geom <- geos::geos_create_rectangle(e, n, e + resolution, n + resolution,
                                      crs = 'epsg:27700')

  if(returnType == 'wkt'){
    return(geos::geos_write_wkt(geom))
  }

  if(returnType == 'geojson'){
    return(geos::geos_write_geojson(geom))
  }

  if(returnType == 'geos'){
    return(geom)
  }

  if(returnType == 'sf'){
    # Check that the `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    } else{
      return(sf::st_as_sf(x = data.frame('grid_ref' = grid_ref),
                          geometry = geom,
                          crs = sf::st_crs(27700)))
    }
  }
}


#' Check that a grid reference conforms to expected BNG format.
#'
#' @param grid_ref Character vector of a BNG grid reference.
#' @details Called primarily for its side effects of throwing an error for
#'   invalid codes.
#' @returns Logical value whether the supplied grid reference meets criteria.
#'
#' @examples
#' osdatahub:::valid_grid_ref('TL63')
#'
#' @keywords internal
#' @noRd
valid_grid_ref <- function(grid_ref){
  valid_ref <- TRUE

  if(!is.character(grid_ref)){
    valid_ref <- FALSE
    stop('Grid reference must be a string.', call. = FALSE)
  }

  if(length(nchar(grid_ref)) > 1L){
    valid_ref <- FALSE
    stop('Plese provide only one grid reference.', call. = FALSE)
  }

  grid_ref <- toupper(grid_ref)
  grid_ref <- gsub(' ', '', grid_ref, fixed = TRUE)

  valid_len <- c(2, 4, 6, 8, 10, 12)
  if(!nchar(grid_ref) %in% valid_len){
    valid_ref <- FALSE
    stop('Invalid grid reference format.', call. = FALSE)
  }

  prefix <- substr(grid_ref, 1, 2)

  if(!substr(prefix, 1, 1) %in% PREFIX1$prefix){
    valid_ref <- FALSE
    stop('Invalid grid reference.', call. = FALSE)
  }

  if(!substr(prefix, 2, 2) %in% PREFIX2$prefix){
    valid_ref <- FALSE
    stop('Invalid grid reference.', call. = FALSE)
  }

  if(nchar(grid_ref) > 2L){
    suffix <- substr(grid_ref, 3, nchar(grid_ref))
    suffix <- suppressWarnings(as.numeric(suffix))

    if(is.na(suffix)){
      valid_ref <- FALSE
      stop('Invalid grid reference.', call. = FALSE)
    }
  }

  return(valid_ref)
}


#' Given a BNG grid reference code, get the implied resolution.
#'
#' @param grid_ref Character vector with a BNG grid reference.
#' @returns Numeric value of the implied resolution.
#'
#' @examples
#' osdatahub:::get_bng_resolution('TL63')
#'
#' @keywords internal
#' @noRd
get_bng_resolution <- function(grid_ref){
  valid_grid_ref(grid_ref)

  grid_ref <- toupper(grid_ref)
  grid_ref <- gsub(' ', '', grid_ref, fixed = TRUE)

  l <- nchar(grid_ref)

  resolution <- 0

  if(l == 2){
    resolution <- 100000
  } else if(l == 4){
    resolution <- 10000
  } else if(l == 6){
    resolution <- 1000
  } else if(l == 8){
    resolution <- 100
  } else if(l == 10){
    resolution <- 10
  } else if(l == 12){
    resolution <- 1
  }

  return(resolution)
}

