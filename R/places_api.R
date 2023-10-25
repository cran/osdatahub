
#' Query the OS Places API
#'
#' Retrieve information on UK addresses within a geographic area or based on a
#' free text search.
#' @param x Either a polygon created with \code{extent_from_*} functions
#'   defining the geographic area, or a character string to search.
#' @param output_crs Output CRS.
#' @param limit (numeric) The maximum number of features to return.
#'   Default is 100 which is the max return per page from the Data Hub.
#' @param classification_code Classification codes to filter query by.
#' @param logical_status_code Logical status code to filter query by.
#' @param minmatch The minimum matching score a result has to be returned.
#' @param matchprecision The decimal point position at which the match score
#'   value is to be truncated.
#' @param dataset (character) The dataset to return. Multiple values can be
#'   provided as a vector. Default is 'DPA'.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Places API provides a detailed view of an address and its
#'   life cycle. Use this function to query Places based on a geographic area or
#'   a free text search.
#'
#'   The Places API contains all the records of AddressBase® Premium and
#'   AddressBase® Premium – Islands and so provides all the information relating
#'   to an address or property from creation to retirement. It contains local
#'   authority, Ordnance Survey and Royal Mail® addresses, current addresses,
#'   and alternatives for current addresses, provisional addresses (such as
#'   planning developments) and historic information, plus OWPAs and cross
#'   references to the OS MasterMap® TOIDS®. OS Places API contains addresses
#'   located within the United Kingdom, Jersey, Guernsey and the Isle of Man.
#'   For address records in Jersey and Guernsey the coordinates will be ‘0.0’ as
#'   they fall outside of the British National Grid. This means they are not
#'   compatible with the GeoSearch operations.
#'
#'   Technical details on the Places API are documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/places/technicalSpecification}.
#'
#'   Note: the Places API requires a Premium API key.
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [extent], [query_nearest_places()], [query_postcode_places()], [query_uprn_places()]
#'
#' @examplesIf has_os_key()
#' # Addresses within a bounding box
#' extent <- extent_from_bbox(c(600000, 310200, 600900, 310900),
#'                            crs = 'EPSG:27700')
#'
#' results <- query_places(extent, limit = 50)
#'
#' # Find addresses by text search.
#' results <- query_places('Ordnance Survey, Adanac Drive, SO16',
#'                         minmatch = 0.5)
#'
#' @import httr
#' @import geojsonsf
#' @import jsonlite
#' @export
query_places <- function(x,
                         ...){

  UseMethod('query_places', x)
}


#' @name query_places
#' @export
query_places.qExtent <- function(x,
                                 output_crs,
                                 limit = 100,
                                 classification_code,
                                 logical_status_code,
                                 dataset = 'DPA',
                                 key = get_os_key(),
                                 returnType = c('geojson', 'list', 'sf'),
                                 ...){
  # Query addresses based on a polygon.

  # Validate parameters.
  params <- list()

  if(limit < 1 | limit > 100){
    stop('Limit must be between 1 and 100.', call. =)
  }

  # qExtent objects should have a CRS.
  input_crs <- x$crs

  if(missing(output_crs)){
    output_crs <- input_crs

    if(is.null(output_crs)){
      stop('Missing output_crs. Please check extent object.',
           call. = FALSE)
    }
  }

  stopifnot(valid_crs(input_crs))
  stopifnot(valid_crs(output_crs))

  params[['srs']] <- toupper(get_crs(input_crs, 'code'))
  params[['output_srs']] <- toupper(get_crs(output_crs, 'code'))

  if(all(dataset %in% c('DPA', 'LPI'))){
    params[['dataset']] <- paste0(unique(dataset), collapse = ',')
  } else{
    stop('Please specify a valid dataset: DPA or LPI.', call. = FALSE)
  }

  returnType <- match.arg(returnType)

  # Add filters.
  params <- places_filter(params, classification_code, logical_status_code)

  # Spatial polygon to filter.
  shp <- geojsonsf::sf_geojson(x$polygon, simplify = TRUE)
  if(is.null(shp)){
    shp <- geos::geos_write_geojson(x$wkt)
  }

  # Build base URL.
  PLACES_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'places', 'url']
  PLACES_ENDPOINT <- paste0(PLACES_ENDPOINT, '/polygon')

  # Set user-agent.
  ua <- httr::user_agent('osdatahub-r')

  # Query loop.
  n_required <- limit
  offset <- 0
  numberReturned <- 0
  data <- list()

  while(n_required > 0){
    limit <- min(n_required, 100)
    offset <- max(offset, numberReturned)
    url <- paste0(PLACES_ENDPOINT, '?key=', key)

    # Update with new offset/limit.
    params[['offset']] <- offset
    params[['maxresults']] <- n_required

    url <- httr::modify_url(url,
                            query = params)

    # Connect to API.
    resp <- httr::POST(url,
                       body = shp,
                       ua,
                       httr::add_headers('Content-Type' = 'application/json'))

    rc <- httr::status_code(resp)

    if (rc != 200) {
      stop(sprintf("OS Places API failed [%s] \nResponse: %s (%s)",
                   rc,
                   RESPONSECODES[RESPONSECODES$code == rc, 'description'],
                   RESPONSECODES[RESPONSECODES$code == rc, 'explanation']),
           call. = FALSE)
    }

    if(httr::http_type(resp) != 'application/json'){
      stop('API did not return json type.', call. = FALSE)
    }

    # Get query results.
    resp_txt <- suppressMessages(httr::content(resp, 'text'))
    results <- process_address_data(resp_txt)

    # Count features
    nReturned <- length(results)
    numberReturned <- numberReturned + nReturned

    # Store results from this loop.
    data <- append(data, results)

    if(nReturned < limit)
      break  # Query complete.
    else
      n_required <- n_required - nReturned

  } # End query loop.

  # Convert list of GeoJSON to FeatureCollection.
  featlist <- list('type' = 'FeatureCollection',
                   'crs' = params[['output_srs']],  # Match format of Python
                   'features' = data)

  featcollect <- jsonlite::toJSON(featlist, auto_unbox = TRUE)

  if(returnType == 'geojson'){
    return(featcollect)
  }

  if(returnType == 'list'){
    return(featlist)
  }

  if(returnType == 'sf'){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    } else{

      # Names of properties. Needed for sorting.
      nms <- names(featlist$features[[1]]$properties)

      # Extract spatial object.
      sdf <- geojsonsf::geojson_sf(featcollect)
      sdf <- sdf[, c(nms, 'geometry')]
      suppressWarnings(sf::st_crs(sdf) <- sf::st_crs(params[['output_srs']]))

      return(sdf)
    }
  }
}


#' @name query_places
#' @export
query_places.character <- function(x,
                                   output_crs = 'EPSG:27700',
                                   limit = 100,
                                   classification_code,
                                   logical_status_code,
                                   minmatch,
                                   matchprecision,
                                   dataset = 'DPA',
                                   key = get_os_key(),
                                   returnType = c('geojson', 'list', 'sf'),
                                   ...){
  # Query addresses based on a free text search.

  # Validate parameters.
  params <- list()

  # Query text.
  params[['query']] <- x

  if(limit < 1 | limit > 100){
    stop('Limit must be between 1 and 100.', call. =)
  }

  if(is.null(output_crs)){
    stop('Missing output_crs. ', call. = FALSE)
  }

  stopifnot(valid_crs(output_crs))
  params[['output_srs']] <- toupper(get_crs(output_crs, 'code'))

  returnType <- match.arg(returnType)

  # Matching parameters.
  if(!missing(minmatch)){
    if(minmatch < 0.1 | minmatch > 1.0){
      stop('`minmatch` setting must be between 0.1 and 1.0', call. = FALSE)
    }
    params[['minmatch']] <- minmatch
  }

  if(!missing(matchprecision)){
    if(matchprecision < 1 | minmatch > 10){
      stop('`matchprecision` setting must be between 1 and 10', call. = FALSE)
    }
  }

  if(all(dataset %in% c('DPA', 'LPI'))){
    params[['dataset']] <- paste0(unique(dataset), collapse = ',')
  } else{
    stop('Please specify a valid dataset: DPA or LPI.', call. = FALSE)
  }

  # Add filters.
  params <- places_filter(params, classification_code, logical_status_code)

  # Execute
  data <- places_query(key, endpoint = 'find', params, limit)

  # Process return data.
  output <- places_process(returnType, data, params[['output_srs']])

  return(output)
}


#' Query the OS Places API
#'
#' Takes a pair of coordinates (X, Y)/(Lon, Lat) as an input to determine the
#' closest address.
#' @param point A set of coordinates as a numeric vector, an object of class
#'   \code{geos}, or an object of class \code{sf}
#' @param point_crs (character or numeric) The identifier for coordinate
#'   reference system information for the point feature.
#' @param radius (numeric) The search radius in metres (max. 1000). Defaults is
#'   100.
#' @param output_crs (character or numeric) The output CRS. Defaults to
#'   “EPSG:27700”.
#' @param classification_code Classification codes to filter query by.
#' @param logical_status_code Logical status code to filter query by.
#' @param dataset (character) The dataset to return. Multiple values can be
#'   provided as a vector. Default is 'DPA'.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Places API provides a detailed view of an address and its
#'   life cycle. Use this function to query Places to find the address nearest
#'   to a given point location.
#'
#'   The Places API contains all the records of AddressBase® Premium and
#'   AddressBase® Premium – Islands and so provides all the information relating
#'   to an address or property from creation to retirement. It contains local
#'   authority, Ordnance Survey and Royal Mail® addresses, current addresses,
#'   and alternatives for current addresses, provisional addresses (such as
#'   planning developments) and historic information, plus OWPAs and cross
#'   references to the OS MasterMap® TOIDS®. OS Places API contains addresses
#'   located within the United Kingdom, Jersey, Guernsey and the Isle of Man.
#'   For address records in Jersey and Guernsey the coordinates will be ‘0.0’ as
#'   they fall outside of the British National Grid. This means they are not
#'   compatible with the GeoSearch operations.
#'
#'   Technical details on the Places API are documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/places/technicalSpecification}.
#'
#'   Note: the Places API requires a Premium API key.
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [query_places()], [query_postcode_places()], [query_uprn_places()]
#'
#' @examplesIf has_os_key()
#' # Find address nearest to a point
#' pt <- c(437292.4, 115541.9)
#'
#' results <- query_nearest_places(pt, point_crs = 'EPSG:27700')
#'
#' @export
query_nearest_places <- function(point,
                                 point_crs,
                                 radius = 100,
                                 output_crs = 'EPSG:27700',
                                 classification_code,
                                 logical_status_code,
                                 dataset = 'DPA',
                                 key = get_os_key(),
                                 returnType = c('geojson', 'list', 'sf'),
                                 ...){
  if(missing(point)){
    stop('Please supply a query point location.', call. = FALSE)
  }

  # Process point location to a vector.
  if(inherits(point, 'numeric')){
    if(length(point) != 2){
      stop('Query points should only contain one coordinate pair.')
    }

    if(missing(point_crs)){
      point_crs <- 'EPSG:27700'
    }

    # Keep as numeric.

  } else if(inherits(point, 'geos_geometry')){
    if(geos::geos_type(point) != 'point'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }

    if(!is.null(attr(point, 'crs'))){
      point_crs <- attr(point, 'crs')
    } else{
      if(missing(point_crs)){
        point_crs <- 'EPSG:27700'
      }
    }

    point <- c(geos::geos_x(point), geos::geos_y(point))

  } else if(inherits(point, c('sf', 'sfc'))){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    }

    if(nrow(point) > 1){
      stop('Please provide only one feature.', call. = FALSE)
    }

    if(sf::st_geometry_type(point) != 'POINT'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }

    point_crs <- sf::st_crs(point)$epsg

    point <- c(sf::st_coordinates(point)[,1],
               sf::st_coordinates(point)[,2])

  } else{
    stop('The query point format is not valid.', call. = FALSE)
  }

  returnType <- match.arg(returnType)

  if(radius < 0.01 | radius > 1000){
    stop('The query radius must be between 0.01 and 1000 metres.',
         call. = FALSE)
  }

  stopifnot(valid_crs(point_crs))
  stopifnot(valid_crs(output_crs))

  point_crs <- get_crs(point_crs, 'code')
  output_crs <- get_crs(output_crs, 'code')

  # Check order of coordinates
  if(toupper(point_crs) == 'EPSG:4326'){
    point <- c(point[2], point[1])
  }

  params <- list('point' = paste(point, collapse = ','),
                 'srs' = point_crs,
                 'output_srs' = output_crs,
                 'radius' = radius)

  if(all(dataset %in% c('DPA', 'LPI'))){
    params[['dataset']] <- paste0(unique(dataset), collapse = ',')
  } else{
    stop('Please specify a valid dataset: DPA or LPI.', call. = FALSE)
  }

  # Add filters.
  params <- places_filter(params, classification_code, logical_status_code)

  # Execute
  data <- places_query(key, endpoint = 'nearest', params, limit = 1)

  # Process return data.
  output <- places_process(returnType, data, params[['output_srs']])

  return(output)
}


#' Query the OS Places API
#'
#' A query of addresses based on a property's postcode.
#' @param postcode The postcode search parameter as a character.
#' @param output_crs (character or numeric) The output CRS. Defaults to
#'   “EPSG:27700”.
#' @param limit (numeric) The maximum number of features to return. Default is
#'   100 which is the max return per page from the Data Hub.
#' @param classification_code Classification codes to filter query by.
#' @param logical_status_code Logical status code to filter query by.
#' @param dataset (character) The dataset to return. Multiple values can be
#'   provided as a vector. Default is 'DPA'.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Places API provides a detailed view of an address and its
#'   life cycle. Use this function to query Places based on a postcode search.
#'   The minimum search parameter for this resource is the postcode area and
#'   postcode district. For example, 'SO16' is a valid search. Full postcodes,
#'   consisting of area, district, sector and unit, e.g. SO16 0AS can also be
#'   supplied.
#'
#'   The Places API contains all the records of AddressBase® Premium and
#'   AddressBase® Premium – Islands and so provides all the information relating
#'   to an address or property from creation to retirement. It contains local
#'   authority, Ordnance Survey and Royal Mail® addresses, current addresses,
#'   and alternatives for current addresses, provisional addresses (such as
#'   planning developments) and historic information, plus OWPAs and cross
#'   references to the OS MasterMap® TOIDS®. OS Places API contains addresses
#'   located within the United Kingdom, Jersey, Guernsey and the Isle of Man.
#'   For address records in Jersey and Guernsey the coordinates will be ‘0.0’ as
#'   they fall outside of the British National Grid. This means they are not
#'   compatible with the GeoSearch operations.
#'
#'   Technical details on the Places API are documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/places/technicalSpecification}.
#'
#'   Note: the Places API requires a Premium API key.
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [query_places()], [query_nearest_places()], [query_uprn_places()]
#'
#' @examplesIf has_os_key()
#' results <- query_postcode_places(postcode = 'SO16 0AS')
#'
#' @export
query_postcode_places <- function(postcode,
                                  output_crs = 'EPSG:27700',
                                  limit = 100,
                                  classification_code,
                                  logical_status_code,
                                  dataset = 'DPA',
                                  key = get_os_key(),
                                  returnType = c('geojson', 'list', 'sf'),
                                  ...){
  if(missing(postcode) | is.null(postcode)){
    stop('A postcode to search must be supplied.', call. = FALSE)
  }

  if(!is.character(postcode)){
    stop('The postcode must be supplied as a character string.', call. = FALSE)
  }

  if(length(postcode) > 1L){
    stop('Please supply only one postcode to search.', call. = FALSE)
  }

  # Validate parameters.
  params <- list()

  # Query text.
  params[['postcode']] <- postcode

  if(limit < 1 | limit > 100){
    stop('Limit must be between 1 and 100.', call. = FALSE)
  }

  if(is.null(output_crs)){
    stop('Missing output_crs. ', call. = FALSE)
  }

  stopifnot(valid_crs(output_crs))
  params[['output_srs']] <- toupper(get_crs(output_crs, 'code'))

  if(all(dataset %in% c('DPA', 'LPI'))){
    params[['dataset']] <- paste0(unique(dataset), collapse = ',')
  } else{
    stop('Please specify a valid dataset: DPA or LPI.', call. = FALSE)
  }

  returnType <- match.arg(returnType)

  # Add filters.
  params <- places_filter(params, classification_code, logical_status_code)

  # Execute
  data <- places_query(key, endpoint = 'postcode', params, limit)

  # Process return data.
  output <- places_process(returnType, data, params[['output_srs']])

  return(output)
}


#' Query the OS Places API
#'
#' A query of addresses based on a property's UPRN.
#' @param uprn A valid UPRN.
#' @param output_crs (character or numeric) The output CRS. Defaults to
#'   “EPSG:27700”.
#' @param classification_code Classification codes to filter query by.
#' @param logical_status_code Logical status code to filter query by.
#' @param dataset (character) The dataset to return. Multiple values can be
#'   provided as a vector. Default is 'DPA'.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Places API provides a detailed view of an address and its
#'   life cycle. Use this function to query Places based on a UPRN search.
#'
#'   The Places API contains all the records of AddressBase® Premium and
#'   AddressBase® Premium – Islands and so provides all the information relating
#'   to an address or property from creation to retirement. It contains local
#'   authority, Ordnance Survey and Royal Mail® addresses, current addresses,
#'   and alternatives for current addresses, provisional addresses (such as
#'   planning developments) and historic information, plus OWPAs and cross
#'   references to the OS MasterMap® TOIDS®. OS Places API contains addresses
#'   located within the United Kingdom, Jersey, Guernsey and the Isle of Man.
#'   For address records in Jersey and Guernsey the coordinates will be ‘0.0’ as
#'   they fall outside of the British National Grid. This means they are not
#'   compatible with the GeoSearch operations.
#'
#'   Technical details on the Places API are documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/places/technicalSpecification}.
#'
#'   Note: the Places API requires a Premium API key.
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [query_places()], [query_nearest_places()], [query_uprn_places()]
#'
#' @examplesIf has_os_key()
#' results <- query_uprn_places(uprn = 200010019924)
#'
#' @export
query_uprn_places <- function(uprn,
                              output_crs = 'EPSG:27700',
                              classification_code,
                              logical_status_code,
                              dataset = 'DPA',
                              key = get_os_key(),
                              returnType = c('geojson', 'list', 'sf'),
                              ...){

  if(missing(uprn) | is.null(uprn)){
    stop('A UPRN to search must be supplied.', call. = FALSE)
  }

  if(length(uprn) > 1L){
    stop('Please supply only one postcode to search.', call. = FALSE)
  }

  # Validate parameters.
  params <- list()

  # Query text.
  params[['uprn']] <- uprn

  if(is.null(output_crs)){
    stop('Missing output_crs. ', call. = FALSE)
  }

  stopifnot(valid_crs(output_crs))
  params[['output_srs']] <- toupper(get_crs(output_crs, 'code'))

  if(all(dataset %in% c('DPA', 'LPI'))){
    params[['dataset']] <- paste0(unique(dataset), collapse = ',')
  } else{
    stop('Please specify a valid dataset: DPA or LPI.', call. = FALSE)
  }

  returnType <- match.arg(returnType)

  # Add filters.
  params <- places_filter(params, classification_code, logical_status_code)

  # Execute
  data <- places_query(key, endpoint = 'uprn', params, limit = 1)

  # Process return data.
  output <- places_process(returnType, data, params[['output_srs']])

  return(output)
}


#' Internal function to add filters to Places API query
#'
#' Combine potential filtering codes into the list of queryables.
#' @param params list of named queryables to modify.
#' @param classification_code Classification codes to filter query by.
#' @param logical_status_code Logical status code to filter query by.
#'
#' @returns List with modified query parameters.
#'
#' @examples
#' params <- list()
#' filter <- osdatahub:::places_filter(params, 'RD02', 1)
#'
#' @keywords internal
#' @noRd
places_filter <- function(params, classification_code, logical_status_code){
  fq_args <- vector('character')

  if(!missing(classification_code)){
    fq_args <- paste0('classification_code:',
                      classification_code,
                      collapse = '+')
  }

  if(!missing(logical_status_code)){
    if(length(logical_status_code) > 1L){
      stop('Logical status code can have a maximum of 1 filter.',
           call. = FALSE)
    }
    lc <- paste0('logical_status_code:', logical_status_code)
    fq_args <- append(fq_args, lc)
  }

  # httr doesn't like multiple parameters with the same name.
  # Avoid URL encoding by wrapping in I(...)
  if(length(fq_args) > 0){
    params[['fq']] <- I(paste0(fq_args, collapse = '&fq='))
  }

  return(params)
}


#' Internal function for querying the Places API
#'
#' Primary helper function for executing queries to the Places API on the OS
#' Data Hub.
#' @param key OS API key.
#' @param endpoint API endpoint to add to the base URL.
#' @param params List of named queryables to filter.
#' @param limit Max results to return.
#'
#' @returns List with the results of the API querys.
#'
#' @examples
#' \dontrun{
#' places_query(get_os_key(), endpoint, params, limit = 1)
#' }
#'
#' @keywords internal
#' @noRd
places_query <- function(key, endpoint, params, limit){

  # Build base URL.
  PLACES_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'places', 'url']
  PLACES_ENDPOINT <- paste0(PLACES_ENDPOINT, '/', endpoint)

  # Set user-agent.
  ua <- httr::user_agent('osdatahub-r')

  # Query loop.
  n_required <- limit
  offset <- 0
  numberReturned <- 0
  data <- list()

  while(n_required > 0){
    limit <- min(n_required, 100)
    offset <- max(offset, numberReturned)
    url <- PLACES_ENDPOINT

    # Update with new offset/limit.
    if(!endpoint %in% c('nearest', 'uprn')){
      params[['offset']] <- offset
      params[['maxresults']] <- n_required
    }

    url <- httr::modify_url(url,
                            query = params)

    # Connect to API.
    resp <- httr::GET(url,
                      ua,
                      httr::add_headers(key = key))

    rc <- httr::status_code(resp)

    if (rc != 200) {
      stop(sprintf("OS Places API failed [%s] \nResponse: %s (%s)",
                   rc,
                   RESPONSECODES[RESPONSECODES$code == rc, 'description'],
                   RESPONSECODES[RESPONSECODES$code == rc, 'explanation']),
           call. = FALSE)
    }

    if(httr::http_type(resp) != 'application/json'){
      stop('API did not return json type.', call. = FALSE)
    }

    # Get query results.
    resp_txt <- suppressMessages(httr::content(resp, 'text'))
    results <- process_address_data(resp_txt)

    # Count features
    nReturned <- length(results)
    numberReturned <- numberReturned + nReturned

    # Store results from this loop.
    data <- append(data, results)

    if(nReturned < limit)
      break  # Query complete.
    else
      n_required <- n_required - nReturned

  } # End query loop.

  return(data)
}


#' Process address data
#'
#' Parse the list of features returned from Places API into GeoJSON format.
#' @param response_text Character vector from the HTTP request.
#'
#' @details This internal function is re-used by the different Places API query
#'   functions to parse the returned JSON text string. Point features are
#'   created and the data are re-formatted as a list object with names that can
#'   be converted into GeoJSON.
#'
#' @returns List of features compatible with GeoJSON specs.
#'
#' @examples
#' \dontrun{
#' resp <- places_query(get_os_key(), endpoint, params, limit = 1)
#' process_address_data(resp)
#' }
#'
#' @keywords internal
#' @noRd
process_address_data <- function(response_text){
  # Extract JSON + Extra members with return information.
  jsonlist <- jsonlite::parse_json(response_text)
  # Count features
  nReturned <- length(jsonlist$results)

  if(nReturned == 0){
    stop('Query returned no features.', call. = FALSE)
  }

  # Names of properties.
  nms <- names(jsonlist$results[[1]][[1]])

  # Find coordinate names. Depends on CRS.
  if(all(c('X_COORDINATE', 'Y_COORDINATE') %in% nms)){
    xnm <- 'X_COORDINATE'
    ynm <- 'Y_COORDINATE'
  } else if(all(c('GEOMETRY_X', 'GEOMETRY_Y') %in% nms)){
    xnm <- 'GEOMETRY_X'
    ynm <- 'GEOMETRY_Y'
  } else if(all(c('LNG', 'LAT') %in% nms)){
    xnm <- 'LNG'
    ynm <- 'LAT'
  } else{
    stop('Expected coordinate names not found.', call. = FALSE)
  }

  # Process features to list compatible with GeoJSON.
  gj <- lapply(jsonlist$results, FUN = function(r){
    feat <- r[[1]]
    fjson <- list('type' = 'Feature',
                  'geometry' = list('type' = 'Point',
                                    'coordinates' = c(feat[[xnm]],
                                                      feat[[ynm]])),
                  'properties' = feat)
    return(fjson)
  })
}


#' Process Address data to FeatureCollection
#'
#' Create the requested output format for Places queries.
#' @param returnType String defining the type of output format.
#' @param data JSON strings of parsed Addres data.
#' @param crs String with the output CRS.
#' @details This internal function is re-used by the different Places API query
#'   functions to parse the returned JSON text string. 'FeatureCollections' are
#'   created and converted into the possible returnType.
#'
#' @returns GeoJSON, list object, or 'sf' object depending on \code{returnType}.
#'
#' @examples
#' \dontrun{
#' resp <- places_query(get_os_key(), endpoint, params, limit = 1)
#' data <- process_address_data(resp)
#' places_process('sf', data, 'EPSG:27700')
#' }
#'
#' @keywords internal
#' @noRd
places_process <- function(returnType, data, crs){

  # Convert list of GeoJSON to FeatureCollection.
  featlist <- list('type' = 'FeatureCollection',
                   'crs' = crs,  # Match format of Python
                   'features' = data)

  featcollect <- jsonlite::toJSON(featlist, auto_unbox = TRUE)

  if(returnType == 'geojson'){
    return(featcollect)
  }

  if(returnType == 'list'){
    return(featlist)
  }

  if(returnType == 'sf'){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    } else{

      # Names of properties. Needed for sorting.
      nms <- names(featlist$features[[1]]$properties)

      # Extract spatial object.
      sdf <- geojsonsf::geojson_sf(featcollect)
      sdf <- sdf[, c(nms, 'geometry')]
      suppressWarnings(sf::st_crs(sdf) <- sf::st_crs(crs))

      return(sdf)
    }
  }
}
