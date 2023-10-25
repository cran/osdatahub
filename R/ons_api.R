
#' Retrieve ONS geographies
#'
#' Query the Office for National Statistics online geography resources.
#' @param ons_code (character) A single ONS code representing a statistical
#'   area.
#' @param returnType (character) Representation for the returned geometry.
#'   Choose \code{'wkt'} or \code{'geojson'} to return the geometry in
#'   Well-Known Text format or GeoJSON, respectively, \code{'geos'} to return an
#'   object of class \code{geos} or \code{'sf'} for a Simple Features object of
#'   class \code{sf}. Default is WKT format.
#' @details The Office for National Statistics (ONS) maintains a source of
#'   official geographies for the UK, such as county boundaries, electoral
#'   wards, parishes, and census output areas. These boundaries are commonly
#'   used for data analysis, particularly of socio-economic factors. A full list
#'   of available ONS geographies can be found here:
#'   \url{https://statistics.data.gov.uk:443/atlas/resource?uri=http://statistics.data.gov.uk/id/statistical-geography/K02000001}.
#'
#'   When returning a \code{geos} object, the coordinate reference system
#'   attribute will be set to CRS code 84 by default and not to a full CRS
#'   definition. The \code{sf} package must be installed in order to return an
#'   object of class \code{sf}.
#'
#'   Data from the ONS are provided under the terms of the Open Government
#'   Licence
#'   \url{https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/}.
#'   For more information, please see:
#'   \url{https://www.ons.gov.uk/methodology/geography/licences}.
#'
#' @returns The coordinates of the polygon boundary in either Well-Known Text
#'   (WKT) format, GeoJSON format, an object of class \code{geos} or as a Simple
#'   Features object of class \code{sf}.
#'
#' @examples
#' \donttest{
#' # Retrieve geography.
#' geog <- get_ons_geom("E05002470")
#' }
#'
#' @import geos
#' @export
get_ons_geom <- function(ons_code,
                         returnType = c('wkt', 'geojson', 'geos', 'sf')){

  if(missing(ons_code)){
    stop('Please provide an ONS area code.', call. = FALSE)
  }

  if(length(ons_code) > 1L){
    stop('Only one ONS code can be queried at a time.', call. = FALSE)
  }

  if(!is.character(ons_code)){
    stop('ONS code must be a character string.', call. = FALSE)
  }

  returnType <- match.arg(returnType)

  ONS_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'ons', 'url']

  url <- paste0(ONS_ENDPOINT, ons_code)

  # Query.
  resp <- httr::GET(url)

  if(httr::http_type(resp) != 'application/json'){
    stop('API did not return json type.', call. = FALSE)
  }

  resp_txt <- httr::content(resp, as = 'text')

  if (httr::status_code(resp) != 200) {
    stop(sprintf("OS NGD API failed [%s]",
                 httr::status_code(resp)),
         call. = FALSE)
  }

  # Get geometry.
  geom <- geos::geos_read_geojson(resp_txt, crs = 'crs84')

  if(geos::geos_is_empty(geom)){
    stop('API did not return a geometry.', call. = FALSE)
  }

  geom <- geos::geos_unnest(geom)

  if(geos::geos_type(geom) != 'polygon'){
    stop(paste0('osdatahub does not currently support geometry types other than Polygon.'),
         call. = FALSE)
  }

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
      sdf <- sf::st_read(resp_txt, quiet = TRUE)
      suppressWarnings(sf::st_crs(sdf) <- sf::st_crs('CRS:84'))
      return(sdf)
    }
  }
}

